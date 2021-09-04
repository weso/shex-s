lazy val scala212 = "2.12.14"
lazy val scala213 = "2.13.6"
lazy val scala3   = "3.0.1-RC2"
lazy val supportedScalaVersions = List(
  scala3,
  scala213,
  scala212,
)

// val Java11 = "adopt@1.11"
val Java8 = "adopt@1.8"

lazy val srdfVersion           = "0.1.104"
lazy val utilsVersion          = "0.1.99"
lazy val documentVersion       = "0.0.33"

// Dependency versions
lazy val antlrVersion          = "4.7.1"
lazy val catsVersion           = "2.6.1"
lazy val catsEffectVersion     = "3.2.7"
lazy val commonsTextVersion    = "1.8"
lazy val declineVersion        = "2.1.0"
lazy val circeVersion          = "0.14.1"
lazy val fs2Version            = "3.0.4"
lazy val jenaVersion           = "4.1.0"
lazy val junitVersion          = "4.13.1"
lazy val junitInterfaceVersion = "0.13.2"
lazy val jgraphtVersion        = "1.4.0"
lazy val munitVersion          = "0.7.27"
lazy val munitEffectVersion    = "1.0.5"
lazy val pprintVersion         = "0.6.6"
lazy val rdf4jVersion          = "3.4.2"
lazy val scalaCollCompatVersion  = "2.4.4"
lazy val scalacheckVersion     = "1.15.4"
lazy val typesafeConfigVersion = "1.4.1"
lazy val xercesVersion         = "2.12.1"

// Dependency modules
lazy val antlr4            = "org.antlr"                  % "antlr4"               % antlrVersion
lazy val catsCore          = "org.typelevel"              %% "cats-core"           % catsVersion
lazy val catsKernel        = "org.typelevel"              %% "cats-kernel"         % catsVersion
lazy val catsEffect        = "org.typelevel"              %% "cats-effect"         % catsEffectVersion
lazy val circeCore         = "io.circe"                   %% "circe-core"          % circeVersion
lazy val circeGeneric      = "io.circe"                   %% "circe-generic"       % circeVersion
lazy val circeParser       = "io.circe"                   %% "circe-parser"        % circeVersion
lazy val decline           = "com.monovore"               %% "decline"             % declineVersion
lazy val declineEffect     = "com.monovore"               %% "decline-effect"      % declineVersion
lazy val fs2               = "co.fs2"            %% "fs2-core" % fs2Version
lazy val fs2io             = "co.fs2"            %% "fs2-io" % fs2Version
lazy val jgraphtCore       = "org.jgrapht"       % "jgrapht-core"     % jgraphtVersion
lazy val jenaArq           = "org.apache.jena"   % "jena-arq"         % jenaVersion
lazy val jenaFuseki        = "org.apache.jena"   % "jena-fuseki-main" % jenaVersion
lazy val junit             = "junit"             % "junit"            % junitVersion
lazy val junitInterface    = "com.github.sbt"    % "junit-interface"  % junitInterfaceVersion
lazy val munit             = "org.scalameta"     %% "munit"           % munitVersion
lazy val munitEffect       = "org.typelevel"     %% "munit-cats-effect-3" % munitEffectVersion
lazy val MUnitFramework = new TestFramework("munit.Framework")

lazy val rdf4j_runtime     = "org.eclipse.rdf4j" % "rdf4j-runtime"    % rdf4jVersion
lazy val scalaCollCompat   = "org.scala-lang.modules"     %% "scala-collection-compat" % scalaCollCompatVersion

// WESO components
lazy val document          = "es.weso"                    %% "document"        % documentVersion
lazy val srdf              = "es.weso"                    %% "srdf"            % srdfVersion
lazy val srdfJena          = "es.weso"                    %% "srdfjena"        % srdfVersion
lazy val srdf4j            = "es.weso"                    %% "srdf4j"          % srdfVersion
lazy val utils             = "es.weso"                    %% "utils"           % utilsVersion
lazy val typing            = "es.weso"                    %% "typing"          % utilsVersion
lazy val validating        = "es.weso"                    %% "validating"      % utilsVersion
lazy val utilsTest         = "es.weso"                    %% "utilstest"       % utilsVersion
lazy val testsuite         = "es.weso"                    %% "testsuite"       % utilsVersion


lazy val scalacheck     = "org.scalacheck"             %% "scalacheck"    % scalacheckVersion
lazy val pprint         = "com.lihaoyi"                %% "pprint"        % pprintVersion
lazy val typesafeConfig = "com.typesafe"               % "config"         % typesafeConfigVersion
lazy val xercesImpl     = "xerces"                     % "xercesImpl"     % xercesVersion

ThisBuild / githubWorkflowJavaVersions := Seq(Java8)

lazy val shexs = project
  .in(file("."))
  .enablePlugins(
    ScalaUnidocPlugin,
    SiteScaladocPlugin,
    AsciidoctorPlugin,
    SbtNativePackager,
    WindowsPlugin,
    JavaAppPackaging,
    LauncherJarPlugin
    )
    .enablePlugins(BuildInfoPlugin)
  .settings(
    commonSettings,
    packagingSettings,
    wixSettings)
  .aggregate(depGraphs, shex, shexTest, rbe, wikibaserdf, shapepath, shapemap, docs)
  .dependsOn(depGraphs, shex, shexTest, rbe, wikibaserdf, shapepath, shapemap)
  .settings(
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      catsEffect,
      decline,
      declineEffect,
      srdf,
      srdf4j,
      srdfJena,
      pprint,
      junitInterface % Test,
    ),
    cancelable in Global := true,
    fork := true,
    ThisBuild / turbo := true,
    ThisBuild / crossScalaVersions := supportedScalaVersions,
    Compile / run / mainClass := Some("es.weso.shexs.Main"),
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "buildinfo"
  )

lazy val CompatTest = config("compat") extend (Test) describedAs ("Tests that check compatibility (some may fail)")
def compatFilter(name: String): Boolean = name endsWith "CompatTest"
def noCompatFilter(name: String): Boolean   = !compatFilter(name)

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
      fs2, fs2io,
      utils     % "test -> test; compile -> compile",
      utilsTest % Test,
      validating,
      srdf,
      xercesImpl,
      srdfJena % Test,
      srdf4j   % Test,
      junit % Test,
      junitInterface % Test,
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
      fs2, fs2io,
      scalaCollCompat,
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
      jgraphtCore,
      munit,
      munitEffect,
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
      antlr4,
      testsuite,
      scalacheck % Test,
      munit % Test,
      munitEffect % Test,
      typesafeConfig % Test,
    ),
    testFrameworks += new TestFramework("munit.Framework")
  ).dependsOn(shex)

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
      srdfJena,
    ),
    testFrameworks ++= Seq(MUnitFramework),
    testOptions.in(Test) += Tests.Argument(MUnitFramework, "--exclude-tags=Slow")
  ).dependsOn(
    shex
  )

lazy val shexTest = project
  .in(file("modules/shexTest"))
  .configs(CompatTest)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    commonSettings,
    inConfig(CompatTest)(Defaults.testTasks),
    testOptions in Test := Seq(Tests.Filter(noCompatFilter)),
    testOptions in CompatTest := Seq(Tests.Filter(compatFilter))
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
      utils     % "test -> test; compile -> compile",
      testsuite,
      srdf,
      srdfJena,
      scalacheck % Test,
      munit % Test,
      munitEffect % Test,
      utilsTest % Test,
      srdf4j % Test,
      typesafeConfig % Test,
    ),
    testFrameworks ++= Seq(
      new TestFramework("munit.Framework"),
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
      srdfJena   % Test,
      utils,
    ) ++ macroDependencies(scalaVersion.value)
  )

lazy val docs = project
  .in(file("shexs-docs"))
  .settings(
    noPublishSettings,
    mdocSettings,
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(noDocProjects: _*)
   )
  .dependsOn(shex, shapemap, rbe, shexTest, wikibaserdf, shapepath, depGraphs)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val mdocSettings = Seq(
  mdocVariables := Map(
    "VERSION" -> version.value
  ),
  ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(rbe, shex, shapemap, shapepath, depGraphs, wikibaserdf),
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
    "-doc-source-url", s"https://github.com/weso/srdf/tree/v${(ThisBuild / version).value}€{FILE_PATH}.scala",
    "-sourcepath", (LocalRootProject / baseDirectory).value.getAbsolutePath,
    "-doc-title", "shex-s",
    "-doc-version", s"v${(ThisBuild / version).value}"
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
   munit % Test,
   munitEffect % Test
  ),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val packagingSettings = Seq(
  mainClass in Compile := Some("es.weso.shexs.Main"),
  mainClass in assembly := Some("es.weso.shexs.Main"),
  test in assembly := {},
  assemblyJarName in assembly := "shex-s.jar",
  packageSummary in Linux := name.value,
  packageSummary in Windows := name.value,
  packageDescription := name.value
)

lazy val compilationSettings = Seq(
  // scalaVersion := "2.13.1",
  // format: off
  // javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-language:_",
//    "-target:jvm-1.8",
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    // "-Xfatal-warnings",
  ) ++ (if (priorTo2_13(scalaVersion.value))
  Seq(
    "-Yno-adapted-args",
    "-Xfuture"
  )
else
  Seq(
    "-Ymacro-annotations"
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
// The same numbers as in the docs?
// wixProductId := "ce07be71-510d-414a-92d4-dff47631848a",
// wixProductUpgradeId := "4552fb0e-e257-4dbd-9ecb-dba9dbacf424"
)

//lazy val ghPagesSettings = Seq(
//  git.remoteRepo := "git@github.com:labra/shaclex.git"
//)

/*lazy val commonSettings = compilationSettings ++ sharedDependencies ++ Seq(
  organization := "es.weso",
  resolvers ++= Seq(
    Resolver.githubPackages("weso"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
  ),
  coverageHighlighting := true,
  githubOwner := "weso",
  githubRepository := "shex-s",
) ++ warnUnusedImport */

def antlrSettings(packageName: String) = Seq(
  antlr4GenListener in Antlr4 := true,
  antlr4GenVisitor in Antlr4 := true,
  antlr4Dependency in Antlr4 := antlr4,
  antlr4PackageName in Antlr4 := Some(packageName)
)

/*lazy val publishSettings = Seq(
  homepage := Some(url("https://github.com/labra/shaclex")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/labra/shaclex"), "scm:git:git@github.com:labra/shaclex.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://labra.github.io/shaclex/latest/api/")),
  pomExtra := <developers>
                       <developer>
                         <id>labra</id>
                         <name>Jose Emilio Labra Gayo</name>
                         <url>https://github.com/labra/</url>
                       </developer>
                     </developers>,
  publishMavenStyle := true,
)*/

lazy val warnUnusedImport = Seq(
 // scalacOptions ++= (if (isDotty.value) Nil else Seq("-Ywarn-unused:imports")),
  scalacOptions in (Compile, console) ~= { _.filterNot(Set("-Ywarn-unused-import", "-Ywarn-unused:imports")) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)

lazy val commonSettings = compilationSettings ++ sharedDependencies ++ Seq(
  coverageHighlighting := priorTo2_13(scalaVersion.value),
  organization := "es.weso",
  sonatypeProfileName := ("es.weso"),
  homepage            := Some(url("https://github.com/weso/shaclex")),
  licenses            := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo             := Some(ScmInfo(url("https://github.com/weso/shaclex"), "scm:git:git@github.com:weso/shaclex.git")),
  autoAPIMappings     := true,
  apiURL              := Some(url("http://weso.github.io/shaclex/latest/api/")),
  autoAPIMappings     := true,
  developers := List(
    Developer(
      id="labra",
      name="Jose Emilio Labra Gayo",
      email="jelabra@gmail.com",
      url=url("https://weso.labra.es")
    ))
) ++ warnUnusedImport
