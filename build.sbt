lazy val scala212 = "2.12.20"
lazy val scala213 = "2.13.15"
lazy val scala3 = "3.6.3"
lazy val supportedScalaVersions = List(
  scala212,
  scala213,
  scala3
)

val Java11 = JavaSpec.temurin("11")

lazy val srdfVersion = "0.1.125"
lazy val utilsVersion = "0.2.25"
lazy val documentVersion = "0.0.34"

// Dependency versions
// lazy val antlrVersion            = "4.9.3"
lazy val catsVersion = "2.9.0"
lazy val catsEffectVersion = "3.4.4"
lazy val circeVersion = "0.14.2"
lazy val commonsTextVersion = "1.8"
lazy val declineVersion = "2.5.0"
lazy val fansiVersion = "0.3.0"
lazy val fs2Version = "3.4.0"
lazy val jenaVersion = "4.3.2"
lazy val junitVersion = "4.13.2"
lazy val junitInterfaceVersion = "0.13.3"
lazy val jgraphtVersion = "1.4.0"
lazy val logbackVersion = "1.2.11"
lazy val munitVersion = "0.7.29" // "1.0.0-M6"
lazy val munitEffectVersion = "1.0.7"
lazy val pprintVersion = "0.7.3"
lazy val rdf4jVersion = "3.4.2"
lazy val scalaCollCompatVersion = "2.8.1"
lazy val scalacheckVersion = "1.15.4"
lazy val scalaLoggingVersion = "3.9.4"
lazy val typesafeConfigVersion = "1.4.2"
lazy val wikidataToolkitVersion = "0.14.7"
lazy val slf4jVersion = "1.7.36"

// Dependency modules
// lazy val antlr4            = "org.antlr"                  % "antlr4"               % antlrVersion
lazy val catsCore = "org.typelevel" %% "cats-core" % catsVersion
lazy val catsKernel = "org.typelevel" %% "cats-kernel" % catsVersion
lazy val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion
lazy val catsAlley = "org.typelevel" %% "alleycats-core" % catsVersion
lazy val circeCore = "io.circe" %% "circe-core" % circeVersion
lazy val circeGeneric = "io.circe" %% "circe-generic" % circeVersion
lazy val circeParser = "io.circe" %% "circe-parser" % circeVersion
lazy val decline = "com.monovore" %% "decline" % declineVersion
lazy val declineEffect = "com.monovore" %% "decline-effect" % declineVersion
lazy val fansi = "com.lihaoyi" %% "fansi" % fansiVersion
lazy val fs2 = "co.fs2" %% "fs2-core" % fs2Version
lazy val fs2io = "co.fs2" %% "fs2-io" % fs2Version
lazy val jgraphtCore = "org.jgrapht" % "jgrapht-core" % jgraphtVersion
lazy val jenaArq = "org.apache.jena" % "jena-arq" % jenaVersion
lazy val jenaFuseki = "org.apache.jena" % "jena-fuseki-main" % jenaVersion
lazy val junit = "junit" % "junit" % junitVersion
lazy val junitInterface = "com.github.sbt" % "junit-interface" % junitInterfaceVersion
lazy val munit = "org.scalameta" %% "munit" % munitVersion
lazy val munitEffect = "org.typelevel" %% "munit-cats-effect-3" % munitEffectVersion
lazy val pprint = "com.lihaoyi" %% "pprint" % pprintVersion
lazy val slf4jAPI = "org.slf4j" % "slf4j-api" % slf4jVersion
lazy val slf4jSimple = "org.slf4j" % "slf4j-simple" % slf4jVersion

lazy val wdtkDumpFiles = "org.wikidata.wdtk" % "wdtk-dumpfiles" % wikidataToolkitVersion
lazy val wdtkBaseApi = "org.wikidata.wdtk" % "wdtk-wikibaseapi" % wikidataToolkitVersion
lazy val wdtkDataModel = "org.wikidata.wdtk" % "wdtk-datamodel" % wikidataToolkitVersion
lazy val wdtkRDF = "org.wikidata.wdtk" % "wdtk-rdf" % wikidataToolkitVersion
lazy val wdtkStorage = "org.wikidata.wdtk" % "wdtk-storage" % wikidataToolkitVersion
lazy val wdtkUtil = "org.wikidata.wdtk" % "wdtk-util" % wikidataToolkitVersion

lazy val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckVersion
lazy val typesafeConfig = "com.typesafe" % "config" % typesafeConfigVersion

lazy val logbackClassic = "ch.qos.logback" % "logback-classic" % logbackVersion
lazy val scalaLogging =
  "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion

lazy val MUnitFramework = new TestFramework("munit.Framework")

lazy val rdf4j_runtime = "org.eclipse.rdf4j" % "rdf4j-runtime" % rdf4jVersion
lazy val scalaCollCompat =
  "org.scala-lang.modules" %% "scala-collection-compat" % scalaCollCompatVersion

// WESO components
lazy val document = "es.weso" %% "document" % documentVersion
lazy val srdf = "es.weso" %% "srdf" % srdfVersion
lazy val srdfJena = "es.weso" %% "srdfjena" % srdfVersion
lazy val srdf4j = "es.weso" %% "srdf4j" % srdfVersion
lazy val utils = "es.weso" %% "utils" % utilsVersion
lazy val typing = "es.weso" %% "typing" % utilsVersion
lazy val validating = "es.weso" %% "validating" % utilsVersion
lazy val utilsTest = "es.weso" %% "utilstest" % utilsVersion
lazy val testsuite = "es.weso" %% "testsuite" % utilsVersion

ThisBuild / githubWorkflowJavaVersions := Seq(Java11)

lazy val shexs = project
  .in(file("."))
  .enablePlugins(
    ScalaUnidocPlugin,
    SiteScaladocPlugin,
    AsciidoctorPlugin,
    // SbtNativePackager,
    WindowsPlugin,
    JavaAppPackaging,
    LauncherJarPlugin
  )
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings, packagingSettings, wixSettings)
  .aggregate(
    depGraphs,
    wshex,
    shex,
    shexTest,
    rbe,
    wikibaserdf,
    shapepath,
    shapemap,
    docs,
    shexsjena
  )
  .aggregate(
    depGraphs,
    wshex,
    shex,
    shexTest,
    rbe,
    wikibaserdf,
    shapepath,
    shapemap,
    docs,
    shexsjena
  )
  .dependsOn(depGraphs, wshex, shex, shexTest, rbe, wikibaserdf, shapepath, shapemap, shexsjena)
  .settings(
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      catsEffect,
      decline,
      declineEffect,
      // slf4jAPI,
      // slf4jSimple,
      srdf,
      srdf4j,
      srdfJena,
      pprint,
      junitInterface % Test
    ),
    cancelable in Global := true,
    fork := true,
    ThisBuild / turbo := true,
    maintainer := "labra@WESO",
    crossScalaVersions := supportedScalaVersions,
    // ThisBuild / crossScalaVersions := supportedScalaVersions,
    // Do not package logback files in .jar, they interfere with other logback
    // files in classpath
    Compile / packageBin / mappings ~= { project =>
      project.filter { case (file, _) =>
        val fileName = file.getName
        !(fileName
          .startsWith("logback") && (fileName.endsWith(".xml") || fileName.endsWith(".groovy")))
      }
    },
    Compile / run / mainClass := Some("es.weso.shexs.Main"),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "buildinfo"
  )

lazy val CompatTest =
  config("compat").extend(Test).describedAs("Tests that check compatibility (some may fail)")
def compatFilter(name: String): Boolean = name.endsWith("CompatTest")
def noCompatFilter(name: String): Boolean = !compatFilter(name)

lazy val shex = project
  .in(file("modules/shex"))
  .enablePlugins(Antlr4Plugin)
  .configs(CompatTest)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    commonSettings,
    antlrSettings("es.weso.shex.parser"),
    inConfig(CompatTest)(Defaults.testTasks),
    Test / testOptions := Seq(Tests.Filter(noCompatFilter)),
    Test / parallelExecution := false,
    CompatTest / testOptions := Seq(Tests.Filter(compatFilter))
  )
  .dependsOn(
    rbe,
    depGraphs,
    shapemap
  )
  .settings(
    libraryDependencies ++= Seq(
      typesafeConfig % Test,
      circeCore,
      circeGeneric,
      circeParser,
      catsEffect,
      pprint,
      scalacheck % Test,
      typing,
      document,
      fs2,
      fs2io,
      utils % "test -> test; compile -> compile",
      utilsTest % Test,
      validating,
      srdf,
      srdfJena % Test,
      srdf4j % Test,
      junit % Test,
      junitInterface % Test
    ),
    testFrameworks += MUnitFramework
  )

lazy val shapemap = project
  .in(file("modules/shapemap"))
  .enablePlugins(Antlr4Plugin)
  .settings(commonSettings, antlrSettings("es.weso.shapemaps.parser"))
  .dependsOn()
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      srdf,
      utils,
      srdfJena % Test,
      // sext % Test,
      catsCore,
      catsKernel,
      circeCore,
      circeGeneric,
      circeParser,
      fs2,
      fs2io,
      scalaCollCompat,
      munit % Test,
      munitEffect % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

lazy val wshex = project
  .in(file("modules/wshex"))
  .enablePlugins(Antlr4Plugin)
  .settings(commonSettings, antlrSettings("es.weso.wshex.parser"))
  .dependsOn(shex)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      utils,
      catsCore,
      catsKernel,
      circeCore,
      circeGeneric,
      circeParser,
      fs2,
      fs2io,
      wdtkBaseApi,
      wdtkDataModel,
      wdtkDumpFiles,
      wdtkRDF,
      wdtkStorage,
      wdtkUtil,
      scalaCollCompat,
      srdfJena,
      munit % Test,
      munitEffect % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

lazy val depGraphs = project
  .in(file("modules/depGraphs"))
  .settings(commonSettings)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      catsAlley,
      jgraphtCore,
      munit % Test,
      munitEffect % Test,
      utils
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )

lazy val shapepath = project
  .in(file("modules/shapepath"))
  .enablePlugins(Antlr4Plugin)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    commonSettings,
    antlrSettings("es.weso.shapepath.parser"),
    libraryDependencies ++= Seq(
      circeCore,
      circeGeneric,
      circeParser,
      catsEffect,
      pprint,
      // antlr4,
      testsuite,
      scalacheck % Test,
      munit % Test,
      munitEffect % Test,
      typesafeConfig % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )
  .dependsOn(shex)

lazy val wikibaserdf = project
  .in(file("modules/wikibaserdf"))
  .settings(commonSettings)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      // catsMacros,
      utils,
      srdf,
      srdfJena
    ),
    testFrameworks ++= Seq(MUnitFramework),
    Test / testOptions += Tests.Argument(MUnitFramework, "--exclude-tags=Slow")
  )
  .dependsOn(
    shex
  )

lazy val shexTest = project
  .in(file("modules/shexTest"))
  .configs(CompatTest)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    commonSettings,
    inConfig(CompatTest)(Defaults.testTasks),
    Test / testOptions := Seq(Tests.Filter(noCompatFilter)),
    CompatTest / testOptions := Seq(Tests.Filter(compatFilter))
  )
  .dependsOn(
    shex,
    shapemap
  )
  .settings(
    libraryDependencies ++= Seq(
      circeCore,
      circeGeneric,
      circeParser,
      catsEffect,
      utils % "test -> test; compile -> compile",
      testsuite,
      srdf,
      srdfJena,
      scalacheck % Test,
      munit % Test,
      munitEffect % Test,
      utilsTest % Test,
      srdf4j % Test,
      typesafeConfig % Test,
      pprint % Test
    ),
    testFrameworks ++= Seq(
      new TestFramework("munit.Framework")
    )
  )

def macroDependencies(scalaVersion: String) =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 =>
      Seq(
        compilerPlugin(("org.scalamacros" %% "paradise" % "2.1.1").cross(CrossVersion.patch))
      )
    case _ => Seq()
  }

lazy val rbe = project
  .in(file("modules/rbe"))
//  .disablePlugins(RevolverPlugin)
  .dependsOn()
  .settings(
    commonSettings
  )
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      validating,
      typing,
      catsCore,
      catsKernel,
      scalacheck % Test,
      srdfJena % Test,
      utils
    ) ++ macroDependencies(scalaVersion.value)
  )

lazy val shexsjena = project
  .in(file("modules/shexsjena"))
//  .disablePlugins(RevolverPlugin)
  .dependsOn(shex)
  .settings(
    commonSettings
  )
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      srdfJena,
      catsEffect,
      utils,
      junitInterface % Test
    ) ++ macroDependencies(scalaVersion.value)
  )

lazy val docs = project
  .in(file("shexs-docs"))
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
  .dependsOn(shex, shapemap, rbe, shexTest, wikibaserdf, shapepath, depGraphs, wshex, shexsjena)
  .settings(
//    scalaVersion := scala213,
    crossScalaVersions := supportedScalaVersions,
    noPublishSettings,
    mdocSettings,
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(noDocProjects: _*)
  )
  .settings(
    // This is based on this question: https://issueexplorer.com/issue/scalameta/mdoc/545
    // mdoc (transitively) depends on sourcecode_2.13,
    // which conflicts with core's dependency on sourcecode_3
    libraryDependencies := libraryDependencies.value.map(
      _.excludeAll(
        ExclusionRule(organization = "com.lihaoyi", name = "sourcecode_2.13"),
        ExclusionRule(organization = "com.lihaoyi", name = "fansi_2.13"),
        ExclusionRule(organization = "com.lihaoyi", name = "pprint_2.13"),
        ExclusionRule(
          organization = "org.scala-lang.modules",
          name = "scala-collection-compat_2.13"
        )
      )
    )
  )

lazy val mdocSettings = Seq(
  mdocVariables := Map(
    "VERSION" -> version.value
  ),
  ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(
    rbe,
    shex,
    shapemap,
    shapepath,
    depGraphs,
    wikibaserdf,
    wshex
  ),
  ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
  cleanFiles += (ScalaUnidoc / unidoc / target).value,
  docusaurusCreateSite := docusaurusCreateSite
    .dependsOn(Compile / unidoc)
    .value,
  docusaurusPublishGhpages :=
    docusaurusPublishGhpages
      .dependsOn(Compile / unidoc)
      .value,
  ScalaUnidoc / unidoc / scalacOptions ++= Seq(
    "-doc-source-url",
    s"https://github.com/weso/srdf/tree/v${(ThisBuild / version).value}€{FILE_PATH}.scala",
    "-sourcepath",
    (LocalRootProject / baseDirectory).value.getAbsolutePath,
    "-doc-title",
    "shex-s",
    "-doc-version",
    s"v${(ThisBuild / version).value}"
  )
)

lazy val noPublishSettings = publish / skip := true

/* ********************************************************
 ******************** Grouped Settings ********************
 **********************************************************/

lazy val noDocProjects = Seq[ProjectReference](
)

lazy val sharedDependencies = Seq(
  libraryDependencies ++= Seq(
    logbackClassic,
    scalaLogging,
    munit % Test,
    munitEffect % Test
  ),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val packagingSettings = Seq(
  Compile / mainClass := Some("es.weso.shexs.Main"),
  assembly / mainClass := Some("es.weso.shexs.Main"),
  assembly / test := {},
  assembly / assemblyJarName := "shex-s.jar",
  Linux / packageSummary := name.value,
  Windows / packageSummary := name.value,
  packageDescription := name.value
)

lazy val compilationSettings = Seq(
  scalaVersion := supportedScalaVersions.head,
  // format: off
  // javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-language:_",
//    "-target:jvm-1.8",
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
//    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    // "-Xfatal-warnings",
  ) ++ (if (priorTo2_13(scalaVersion.value))
  Seq(
    "-Yno-adapted-args",
    "-Xfuture"
  )
else
  Seq(
//    "-Ymacro-annotations"
  ))

  // format: on
)

def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

lazy val wixSettings = Seq(
  wixProductId := "39b564d5-d381-4282-ada9-87244c76e14b",
  wixProductUpgradeId := "6a710435-9af4-4adb-a597-98d3dd0bade1"
)

def antlrSettings(packageName: String) = Seq(
  Antlr4 / antlr4Version := "4.9.3",
  Antlr4 / antlr4GenListener := true,
  Antlr4 / antlr4GenVisitor := true,
  Antlr4 / antlr4PackageName := Some(packageName)
)

lazy val warnUnusedImport = Seq(
  Compile / console / scalacOptions ~= {
    _.filterNot(Set("-Ywarn-unused-import", "-Ywarn-unused:imports"))
  },
  Test / console / scalacOptions := (Compile / console / scalacOptions).value
)

lazy val commonSettings = compilationSettings ++ sharedDependencies ++ Seq(
  coverageHighlighting := priorTo2_13(scalaVersion.value),
  organization := "es.weso",
  sonatypeProfileName := "es.weso",
  homepage := Some(url("https://github.com/weso/shex-s")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(
    ScmInfo(url("https://github.com/weso/shex-s"), "scm:git:git@github.com:weso/shex-s.git")
  ),
  autoAPIMappings := true,
  apiURL := Some(url("http://weso.github.io/shex-s/latest/api/")),
  autoAPIMappings := true,
  developers := List(
    Developer(
      id = "labra",
      name = "Jose Emilio Labra Gayo",
      email = "jelabra@gmail.com",
      url = url("https://weso.labra.es")
    )
  )
  /*libraryDependencies += compilerPlugin(
    ("com.github.ghik" % "zerowaste" % "1.0.0").cross(CrossVersion.full)
  )*/
) ++ warnUnusedImport
