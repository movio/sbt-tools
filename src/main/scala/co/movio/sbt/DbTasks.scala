package co.movio.sbt

import java.lang.ClassLoader

import sbt.Classpaths.managedJars
import sbt.Classpaths.managedJars
import sbt.Defaults._
import sbt.Keys._
import sbt.Load.BuildStructure
import sbt.Project.Initialize
import sbt._
import sbt.complete.DefaultParsers._
import sbt.complete.Parser

object DbTasksPlugin extends DbTasks
trait DbTasks extends Plugin {

  case class DbSchema(
    port: Int,
    project: String,
    ddl: String,
    createIdbSchema: Boolean = false,
    customSqlFiles: String = "")

  val dbClean = TaskKey[Unit]("dbclean")

  val dbInit = TaskKey[Unit]("dbinit", "Checks if the dbtest file has changed since last deploy. If so it redeploy")

  val schemasToCreate = SettingKey[Int]("schemasToCreate", "the number of schemas dbInit should create, default 2")

  val dbSchemas = SettingKey[Seq[DbSchema]]("dbSchema", "tbc")

  val includeNextReleaseMigrations = SettingKey[Boolean]("includeNextReleaseMigration", "Enable this to include the migrations scripts in the 'next-release' directory.")

  val dbTasks = Seq(
    schemasToCreate := 1, // default to 2
    dbSchemas := Seq.empty,
    includeNextReleaseMigrations := false,
    dbInit <<= (baseDirectory, streams, testLoader in Test, fullClasspath in Compile, schemasToCreate, dbSchemas, includeNextReleaseMigrations, classpathTypes, update) map {
      (base, s, loader, cp, numSchemas, dbSchemas, includeNextReleaseMigrations, ct, up) ⇒
        {
          s.log.info("Starting DB setup for " + dbSchemas)
          s.log.info("loader " + loader)
          val propsName = "flyway.properties"
          var flywayConf = base / ".." / propsName

          // Create flyway.properties if they don't exist
          if (!flywayConf.exists) {
            val in = loader.getResourceAsStream(propsName)
            IO.transfer(in, flywayConf)
          }

          dbSchemas foreach { initSchema(_, base, flywayConf, includeNextReleaseMigrations, numSchemas, loader) }
        }
    }
  )

  def initSchema(db: DbSchema,
                 base: File,
                 flywayConf: File,
                 includeNextReleaseMigrations: Boolean,
                 numSchemas: Int,
                 loader: ClassLoader): Unit = {
    // Only run if files have changed
    // md5 files in backend/src/main/resources/db-bonc/migration/
    //              backend/src/main/resources/db-bonc/next-release/
    val migrationFiles = IO.listFiles(base / ("../" + db.project + "/src/main/resources/db-" + db.ddl + "/migration"))
    val nextReleaseFiles = IO.listFiles(base / ("../" + db.project + "/src/main/resources/db-" + db.ddl + "next-release"))
    val allFiles = migrationFiles ++ nextReleaseFiles
    val byteList = allFiles.map(IO.readBytes)
    val bytes = Array.concat(byteList: _*)

    val jarMd5 = java.security.MessageDigest.getInstance("MD5").digest(bytes).map("%02X" format _).mkString

    val lines = IO.readLines(flywayConf)
    val oldMd5 = lines find (_ startsWith ("custom." + db.ddl + ".dbtest-md5=")) map (_.dropWhile(_ != '=').drop(1))
    val oldLocations = lines find (_ startsWith ("custom." + db.ddl + ".locations=")) map (_.dropWhile(_ != '=').drop(1))

    val locations =
      if (includeNextReleaseMigrations) Seq("db-" + db.ddl + "/migration", "db-" + db.ddl + "/next-release")
      else Seq("db-" + db.ddl + "/migration")

    val dbAlreadyCreated: Boolean = (
      for {
        oldMd5 ← oldMd5
        oldLocations ← oldLocations
      } yield oldMd5 == jarMd5 && oldLocations.split(',').toSeq == locations
    ).getOrElse(false)

    if (dbAlreadyCreated) {
      println("DB already created to the current version for: " + db.ddl)
    } else {
      println("Initialising the DB for schema: " + db.ddl)

      // If there was an old configuration, remove it
      oldMd5 foreach { _ ⇒
        // Overwrite flyway.props with out config lines
        val newProps = lines.filter(line ⇒ !(line.startsWith("custom." + db.ddl + ".")))
        IO.writeLines(flywayConf, newProps)
      }

      val time = new java.util.Date().getTime
      val schemas = (1 to numSchemas) map (n ⇒ "vc_" + db.ddl + "_" + time + "_" + n)

      // Append schema info
      val schema = "custom." + db.ddl + ".schemas=" + schemas.mkString(",") + "\n"
      val location = "custom." + db.ddl + ".locations=" + locations.mkString(",") + "\n"
      val port = "custom." + db.ddl + ".port=" + db.port + "\n"
      IO.append(flywayConf, schema)
      IO.append(flywayConf, location)
      IO.append(flywayConf, port)
      IO.append(flywayConf, "custom." + db.ddl + ".dbtest-md5=" + jarMd5 + "\n")

      // Hack to setup infinidb schema
      val customSchema = if (db.createIdbSchema) {
        schemas(0).replaceFirst("^vc_", "idb_") + ":3307"
      } else ""

      // Little hack to send the correct classloader to our class
      val clazz = loader.loadClass("atm.db.SbtToolsInit")
      val method = clazz.getDeclaredMethod("init",
        classOf[java.lang.ClassLoader], classOf[String], classOf[String], classOf[String])

      method.invoke(clazz.newInstance, loader, db.ddl, db.customSqlFiles, customSchema)
      println("Finished setting up DB")
    }
  }
  dbClean <<= (baseDirectory, streams, testLoader in Test) map {
    (base, streams, loader) ⇒
      {
        streams.log.info("Note: this does not drop schemas in your MySQL.")
        val propsName = "flyway.propertie"
        val flywayConf = base / ".." / propsName
        if (flywayConf.exists) {
          streams.log.info("Removing flyway.properties...")
          IO.delete(flywayConf)
        } else {
          streams.log.info("flyway.properties already deleted.")
        }
      }
  }
}
