package co.movio.sbt

import sbt.Classpaths.managedJars
import sbt.Classpaths.managedJars
import sbt.Defaults._
import sbt.Keys._
import sbt.Load.BuildStructure
import sbt.Project.Initialize
import sbt._
import sbt.complete.DefaultParsers._
import sbt.complete.Parser

case class Deps(
  modules: Set[String] = Set.empty, // Other modules that we depend on
  deps: Set[ModuleID] = Set.empty, // Deps that should be transitive, can include test deps
  testModules: Set[String] = Set.empty, // Local modules in test scope
  intransitiveModules: Set[String] = Set.empty) // Local modules that should be imported intransitivly

trait MovioDependenciesPlugin {
  type ProjDependencies = Map[String, Deps]

  // Need to be declared in implementation
  val ProjectOrg: String
  val projectDependencies: ProjDependencies

  lazy val dependsOnMe = TaskKey[Unit]("dependsOnMe")
  lazy val iDependOn = TaskKey[Unit]("iDependOn")
  lazy val graphDepsTask = TaskKey[Unit]("graphDeps")
  lazy val graphDepsReverseTask = TaskKey[Unit]("graphDepsReverse")
  lazy val dependencyTasks = Seq(
    dependsOnMe <<= (projectID, streams) map { (module, streams) ⇒
      {
        streams.log.info("Depends on %s: ".format(module.name))
        getWhatDependsOnMe(module.name, depsMap_all) foreach (streams.log.info(_))
      }
    },
    iDependOn <<= (projectID, streams) map { (module, streams) ⇒
      {
        streams.log.info("(%s) depends on:".format(module.name))
        getWhatIDependOn(module.name, depsMap_all) foreach (streams.log.info(_))
      }
    },
    graphDepsTask <<= (projectID, streams) map { (module, streams) ⇒
      {
        val graphName = module.name + "_dependencies"
        val graphBody = graphDeps(module.name, depsMap, depsMap_test).mkString(";")
        val dotFile = "digraph " + graphName + "{" + "graph[nodesep=0.2,ranksep=0.7]" + graphBody + "}"
        val graphDirectory = "dependencygraphs"
        val graphFileName = graphDirectory + "/" + graphName + ".svg"

        val res = if (Seq("mkdir", "-p", graphDirectory).! == 0)
          Seq("echo", dotFile) #| Seq("dot", "-Tsvg", "-o", graphFileName) !
        else 1
        if (res == 0)
          streams.log.info("(%s) dependency graph generated at (%s)".format(module.name, graphFileName))
        else
          streams.log.info("(%s) dependency graph generation failed".format(module.name))
      }
    },
    graphDepsReverseTask <<= (projectID, streams) map { (module, streams) ⇒
      {
        val graphName = module.name + "_dependencies_reverse"
        val graphBody = graphDepsReverse(module.name, depsMap, depsMap_test).mkString(";")
        val dotFile = "digraph " + graphName + "{" + "graph[nodesep=0.2,ranksep=0.7]" + graphBody + "}"
        val graphDirectory = "dependencygraphs"
        val graphFileName = graphDirectory + "/" + graphName + ".svg"

        val res = if (Seq("mkdir", "-p", graphDirectory).! == 0)
          Seq("echo", dotFile) #| Seq("dot", "-Tsvg", "-o", graphFileName) !
        else 1
        if (res == 0)
          streams.log.info("(%s) reverse dependency graph generated at (%s)".format(module.name, graphFileName))
        else
          streams.log.info("(%s) reverse dependency graph generation failed".format(module.name))
      }
    }
  )

  def getSubModuleDeps(name: String): Seq[ModuleID] =
    projectDependencies(name).modules.toSeq map { module ⇒
      movioModule(module)
    }

  def getTestSubModuleDeps(name: String): Seq[ModuleID] =
    projectDependencies(name).testModules.toSeq map { module ⇒
      movioModule(module, test = true)
    }

  def getIntransivitiveSubModuleDeps(name: String): Seq[ModuleID] =
    projectDependencies(name).intransitiveModules.toSeq map { module ⇒
      movioModule(module, intransitive = true)
    }

  lazy val depsMap: Map[String, Set[String]] = projectDependencies.map { case (k, v) ⇒ (k, v.modules) } withDefaultValue (Set.empty[String])
  lazy val depsMap_test: Map[String, Set[String]] = projectDependencies.map { case (k, v) ⇒ (k, v.testModules) } withDefaultValue (Set.empty[String])
  lazy val depsMap_all: Map[String, Set[String]] = projectDependencies.map { case (k, v) ⇒ (k, v.modules ++ v.testModules) } withDefaultValue (Set.empty[String])

  def reverseDepsMap(map: Map[String, Set[String]]): Map[String, Set[String]] =
    map.toList flatMap {
      case (k, vs) ⇒ vs.map(v ⇒ (v, k))
    } groupBy (_._1) map {
      case (k, kvs) ⇒ (k, kvs.map(_._2).toSet)
    } withDefaultValue (Set.empty[String])

  def getWhatDependsOnMe(module: String, deps: Map[String, Set[String]]): List[String] =
    getWhatIDependOn(module, reverseDepsMap(deps))

  def getWhatIDependOn(module: String, deps: Map[String, Set[String]]): List[String] =
    addModules(deps(module), List(), List(), deps).right.get

  def getModuleOrder(deps: Map[String, Set[String]]): List[String] =
    addModules(deps.keySet, List(), List(), deps).right.get

  def getModulesBelowMe(module: String, deps: Map[String, Set[String]]): List[String] = {
    val allDeps = addModules(deps.keySet, List(), List(), deps).right.get
    allDeps.takeWhile(s ⇒ s != module)
  }

  def addModule(module: String, order: List[String], path: List[String], deps: Map[String, Set[String]]): Either[Exception, List[String]] =
    if (path.contains(module)) Left(
      new Exception("Circular dependency while adding module '" + module + "'! Path: " + path.reverse.reduceLeft(_ + " <-" + _)))
    else if (order.contains(module)) Right(order)
    else addModules(deps(module), order, module :: path, deps) match {
      case Right(o) ⇒ Right(module :: o)
      case Left(ex) ⇒ Left(ex)
    }

  def addModules(modules: Set[String], order: List[String], path: List[String], deps: Map[String, Set[String]]): Either[Exception, List[String]] = modules.toList match {
    case Nil ⇒ Right(order)
    case d :: ds ⇒ addModule(d, order, path, deps) match {
      case Right(o) ⇒ addModules(ds.toSet, o, path, deps)
      case Left(ex) ⇒ Left(ex)
    }
  }

  def getWhatIDependOn_excludeTransitives(module: String, deps: Map[String, Set[String]]): Set[String] = {
    val iDependOn = deps(module)
    iDependOn -- iDependOn.flatMap { mod: String ⇒ getWhatIDependOn(mod, deps) }
  }

  def getWhatTestsIDependOn_excludeTransitives(module: String, deps: Map[String, Set[String]], testDeps: Map[String, Set[String]]): Set[String] = {
    val testsIDependOn = testDeps(module)
    val iDependOn = getWhatIDependOn(module, deps)
    testsIDependOn -- testsIDependOn.flatMap { mod: String ⇒ getWhatIDependOn(mod, deps) } -- iDependOn
  }

  def getWhatDependsOnMeForTests_excludeTransitives(module: String, deps: Map[String, Set[String]], testDeps: Map[String, Set[String]]): Set[String] = {
    val reverseDeps = reverseDepsMap(deps)
    val reverseTestDeps = reverseDepsMap(testDeps)

    val dependsOnMeForTests = reverseTestDeps(module)
    val dependsOnMe = getWhatDependsOnMe(module, deps)
    dependsOnMeForTests -- dependsOnMe.flatMap { mod: String ⇒ reverseTestDeps(mod) } -- dependsOnMe
  }

  def graphDeps(module: String, deps: Map[String, Set[String]], testDeps: Map[String, Set[String]]): Set[String] = {
    val dependencies = getWhatIDependOn_excludeTransitives(module, deps)
    val redundantDependencies = deps(module) -- dependencies

    val testDependencies = getWhatTestsIDependOn_excludeTransitives(module, deps, testDeps)
    val redundantTestDependencies = testDeps(module) -- testDependencies

    val thisModule = dependencies.map(_ + " -> " + module + """[color="blue",weight=3]""") ++
      redundantDependencies.map(_ + " -> " + module + """[color="red",weight=1,constraint=false]""") ++
      testDependencies.map(_ + " -> " + module + """[color="green",weight=1,constrait=true]""") ++
      redundantTestDependencies.map(_ + " -> " + module + """[color="orange",weight=1,constrait=false]""")

    val allDependencies = dependencies ++ testDependencies

    (allDependencies flatMap { graphDeps(_, deps, testDeps) }) ++ thisModule
  }

  def graphDepsReverse(module: String, deps: Map[String, Set[String]], testDeps: Map[String, Set[String]]): Set[String] = {
    val reverseDeps = reverseDepsMap(deps)
    val reverseTestDeps = reverseDepsMap(testDeps)

    val dependencies = getWhatIDependOn_excludeTransitives(module, reverseDeps)
    val redundantDependencies = reverseDeps(module) -- dependencies

    val testDependencies = getWhatDependsOnMeForTests_excludeTransitives(module, deps, testDeps)
    val redundantTestDependencies = reverseTestDeps(module) -- testDependencies

    val thisModule = dependencies.map(module + " -> " + _ + """[color="blue",weight=3]""") ++
      redundantDependencies.map(module + " -> " + _ + """[color="red",weight=1,constraint=false]""") ++
      testDependencies.map(module + " -> " + _ + """[color="green",weight=1,constraint=true]""") ++
      redundantTestDependencies.map(module + " -> " + _ + """[color="orange",weight=1,constraint=false]""")

    val allDependencies = dependencies ++ testDependencies

    (allDependencies flatMap { graphDepsReverse(_, deps, testDeps) }) ++ thisModule
  }

  def graphAllDeps(deps: Map[String, Set[String]], testDeps: Map[String, Set[String]]): Set[String] =
    deps.keySet flatMap { graphDeps(_, deps, testDeps) }

  def getVersion(module: String): String = {
    val deps = getWhatIDependOn(module, depsMap_all).toSet
    val sha = getGitSha(module, deps).take(7) + "-" + getDepsSha(module, deps).take(7)
    if (isModuleSnapshot(module, deps))
      sha + "-SNAPSHOT"
    else
      sha
  }

  def getDepsSha(module: String, deps: Set[String]): String =
    Hash.toHex(Hash(deps.toSeq.sorted.mkString))

  def getGitSha(module: String, deps: Set[String]): String =
    Process("git log -1 --format=%%H %s".format((deps + module).mkString(" "))).!!.trim

  def isModuleSnapshot(module: String, deps: Set[String]): Boolean =
    Process("git status -s %s".format((deps + module).mkString(" "))).lines.nonEmpty

  def movioModule(module: String, test: Boolean = false, intransitive: Boolean = false): ModuleID = {
    val dep = ProjectOrg %% module % getVersion(module)
    val tested = if (test) dep % "test"
    else dep
    if (intransitive) tested intransitive ()
    else tested
  }

  lazy val graphAllDepsTask = TaskKey[Unit]("graphAllDeps")
  lazy val rootDependencyTasks = Seq(
    graphAllDepsTask <<= (projectID, streams) map { (module, streams) ⇒
      {
        val graphName = "all_dependencies"
        val graphBody = graphAllDeps(depsMap, depsMap_test).mkString(";")
        val dotFile = "digraph " + graphName + "{" + "graph[nodesep=0.2,ranksep=0.7]" + graphBody + "}"
        val graphDirectory = "dependencygraphs"
        val graphFileName = graphDirectory + "/" + graphName + ".svg"

        val res = if (Seq("mkdir", "-p", graphDirectory).! == 0)
          Seq("echo", dotFile) #| Seq("dot", "-Tsvg", "-o", graphFileName) !
        else 1
        if (res == 0)
          streams.log.info("full dependency graph generated at (%s)".format(graphFileName))
        else
          streams.log.info("full dependency graph generation failed")
      }
    }
  )

  lazy val checkVersionsTask = TaskKey[Unit]("checkVersions")
  lazy val checkVersionsSettings = Seq(
    checkVersionsTask <<= (streams, buildStructure, projectID) map {
      (streams, structure, module) ⇒
        val version = getVersion(module.name)
        if ((!module.revision.endsWith("-SNAPSHOT")) && module.revision != version)
          fail("Version out of date - reload sbt (" + module.revision + " != " + version + ")")
    }
  )

  lazy val doAbove = InputKey[Any]("doAbove", "Performs a given commands for its dependencies and itself, in proper order.")
  lazy val doBelow = InputKey[Any]("doBelow", "Performs a given commands on itself and modules that depend on it, in proper order.")
  lazy val resume = InputKey[Any]("resume", "Performs a given command on all modules after this module in the full build process, in proper order.")
  lazy val resumeUntil = InputKey[Any]("resumeUntil", "Performs a given command on all modules after this module in the full build process, in proper order. Stops at the module with first parameter")
  lazy val retry = InputKey[Any]("retry", "Performs a given command on all modules including and after this module in the full build process, in proper order.")
  lazy val retryUntil = InputKey[Any]("retryUntil", "Performs a given command on all modules including and after this module in the full build process, in proper order. Stops at the module with first parameter")
  lazy val doAboveBelowSettings = Seq(
    doAbove <<= doOnModulesTask { module ⇒ getWhatIDependOn(module, depsMap_all).reverse :+ module },
    doBelow <<= doOnModulesTask { module ⇒ module :: getWhatDependsOnMe(module, depsMap_all) },
    resume <<= doOnModulesTask { module ⇒ getModulesBelowMe(module, depsMap_all).reverse },
    resumeUntil <<= doOnModulesUntilTask { module ⇒ getModulesBelowMe(module, depsMap_all).reverse },
    retry <<= doOnModulesTask { module ⇒ (getModulesBelowMe(module, depsMap_all) :+ module).reverse },
    retryUntil <<= doOnModulesUntilTask { module ⇒ (getModulesBelowMe(module, depsMap_all) :+ module).reverse }
  )
  lazy val doAll = InputKey[Any]("doAll", "Performs a given command for all modules, in proper order.")
  lazy val doAllUntil = InputKey[Any]("doAllUntil", "Performs a given command for all modules, but stops before the first argument: eg doAllUntil core test.")
  lazy val doAllSettings = Seq(
    doAll <<= doOnModulesTask { projectDependencies ⇒ getModuleOrder(depsMap_all).reverse },
    doAllUntil <<= doOnModulesUntilTask { projectDependencies ⇒ getModuleOrder(depsMap_all).reverse }
  )

  lazy val readyToPush = TaskKey[Any]("readyToPush", "Alias for `doAll testCompile publishLocal`.")
  lazy val readyToPushSettings = Seq(
    readyToPush <<= (streams, buildStructure, state) map {
      (streams, structure, state) ⇒
        val modules: List[String] = getModuleOrder(depsMap_all).reverse
        val success = evaluateTasksOnModules(modules, List("testCompile", "publishLocal"), state, structure, streams)
        if (!success) fail("A task failed - stopping.")
    }
  )

  def doOnModulesTask(getModules: String ⇒ List[String]): Initialize[InputTask[Any]] =
    InputTask(doParser) { (argTask: TaskKey[Seq[String]]) ⇒
      (projectID, streams, argTask, buildStructure, state) map {
        (module, streams, tasks, structure, state) ⇒
          val modules: List[String] = getModules(module.name)

          val success = evaluateTasksOnModules(modules, tasks.toList, state, structure, streams)
          if (!success) fail("A task failed - stopping.")
      }
    }

  def doOnModulesUntilTask(getModules: String ⇒ List[String]): Initialize[InputTask[Any]] =
    InputTask(doUntilParser) { (argTask: TaskKey[(String, Seq[String])]) ⇒
      (projectID, streams, argTask, buildStructure, state) map {
        (module, streams, args, structure, state) ⇒
          val excludeFrom = args._1
          val tasks = args._2

          val allModules: List[String] = getModules(module.name)

          val modules = allModules.takeWhile(_ != excludeFrom)
          println(excludeFrom )
          println(modules)
          val success = evaluateTasksOnModules(modules, tasks, state, structure, streams)
          if (!success) fail("A task failed - stopping.")
      }
    }

  def evaluateTasksOnModules(modules: Seq[String], tasks: Seq[String], state: State, structure: BuildStructure, streams: TaskStreams): Boolean = modules match {
    case head :: tail ⇒
      if (evaluateTasksOnModule(head, tasks, state, structure, streams))
        evaluateTasksOnModules(tail, tasks, state, structure, streams)
      else false
    case Nil ⇒ true
  }

  def evaluateTasksOnModule(module: String, tasks: Seq[String], state: State, structure: BuildStructure, streams: TaskStreams): Boolean = tasks match {
    case head :: tail ⇒
      if (evaluateTaskOnModule(module, head, state, structure, streams))
        evaluateTasksOnModule(module, tail, state, structure, streams)
      else false
    case Nil ⇒ true
  }

  val doParser: State ⇒ Parser[Seq[String]] = (state: State) ⇒
    token(
      (Space ~> "clean") |
        (Space ~> "compile") |
        (Space ~> "publishLocal") |
        (Space ~> "testCompile") |
        (Space ~> "test") |
        (Space ~> "list") |
        (Space ~> "doNotEverEverDoThisLocallyOnlyForUseWithJenkins")
    ).+

  val doUntilParser: State ⇒ Parser[(String, Seq[String])] = (state: State) ⇒
    Space ~> StringBasic.examples("module") ~ 
      token(
     //(Space ~> StringBasic.examples("module")) |
        (Space ~> "clean") |
          (Space ~> "compile") |
          (Space ~> "publishLocal") |
          (Space ~> "testCompile") |
          (Space ~> "test") |
          (Space ~> "list") |
          (Space ~> "doNotEverEverDoThisLocallyOnlyForUseWithJenkins")
        ).+

  def evaluateTaskOnModule(module: String, task: String, state: State, structure: BuildStructure, streams: TaskStreams): Boolean = {
    object tasks {
      val clean: ScopedKey[Task[Unit]] = Keys.clean
      val compile: ScopedKey[Task[sbt.inc.Analysis]] = Keys.compile in Compile
      val publishLocal: ScopedKey[Task[Unit]] = Keys.publishLocal
      val testCompile: ScopedKey[Task[sbt.inc.Analysis]] = Keys.compile in Test
      val test: ScopedKey[Task[Unit]] = Keys.test.in(Test)
      val publish: ScopedKey[Task[Unit]] = Keys.publish
      val list = "list"
    }

    val projectRefs = structure.allProjectRefs.map(pr ⇒ pr.project → pr).toMap

    def evaluateTask[A](key: ScopedKey[Task[A]]): Boolean =
      EvaluateTask(Project.extract(state).structure, key, state, projectRefs(module), EvaluateTask defaultConfig state) match {
        case Some((_, Value(_))) ⇒
          streams.log.info("😸	Command " + task + " on " + module + " succeeded.")
          true
        case Some((_, Inc(_))) | None ⇒
          streams.log.info("😿	Command " + task + " on " + module + " failed.")
          false
      }

    task match {
      case "clean" ⇒ evaluateTask(tasks.clean)
      case "compile" ⇒ evaluateTask(tasks.compile)
      case "publishLocal" ⇒ evaluateTask(tasks.publishLocal)
      case "testCompile" ⇒ evaluateTask(tasks.testCompile)
      case "test" ⇒ evaluateTask(tasks.test)
      case "doNotEverEverDoThisLocallyOnlyForUseWithJenkins" ⇒ evaluateTask(tasks.publish)
      case "list" ⇒ println(module); true
    }
  }
}
