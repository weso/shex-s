lazy val scala212 = "2.12.12"
lazy val scala213 = "2.13.3"
lazy val supportedScalaVersions = List(scala213, scala212)

// Local dependencies
lazy val srdfVersion           = "0.1.83"
lazy val shapeMapsVersion      = "0.1.67"
lazy val utilsVersion          = "0.1.73"
lazy val documentVersion       = "0.0.11"

// Dependency versions
lazy val antlrVersion          = "4.7.1"
lazy val catsVersion           = "2.3.0"
lazy val catsEffectVersion     = "2.3.0"
lazy val commonsTextVersion    = "1.8"
lazy val console4catsVersion   = "0.8.1"
lazy val circeVersion          = "0.14.0-M1"
lazy val declineVersion        = ""
lazy val diffsonVersion        = "4.0.0"
lazy val fs2Version            = "2.4.0"
// lazy val effVersion            = "4.6.1"
lazy val jenaVersion           = "3.16.0"
lazy val junitVersion          = "4.13.1"
lazy val junitInterfaceVersion = "0.11"
lazy val jgraphtVersion        = "1.3.1"
lazy val logbackVersion        = "1.2.3"
lazy val loggingVersion        = "3.9.2"
lazy val pprintVersion         = "0.5.9"
lazy val rdf4jVersion          = "3.4.2"
lazy val scalacheckVersion     = "1.14.0"
lazy val scalacticVersion      = "3.2.0"
lazy val scalaTestVersion      = "3.2.0"
lazy val scalaGraphVersion     = "1.11.5"
// lazy val scalatagsVersion      = "0.6.7"
lazy val scallopVersion        = "3.3.1"
lazy val sextVersion           = "0.2.6"
lazy val typesafeConfigVersion = "1.3.4"
lazy val xercesVersion         = "2.12.0"

// Compiler plugin dependency versions
lazy val simulacrumVersion = "1.0.0"
// lazy val kindProjectorVersion = "0.9.5"
lazy val scalaMacrosVersion = "2.1.1"

// Dependency modules
lazy val antlr4            = "org.antlr"                  % "antlr4"               % antlrVersion
lazy val catsCore          = "org.typelevel"              %% "cats-core"           % catsVersion
lazy val catsKernel        = "org.typelevel"              %% "cats-kernel"         % catsVersion
// lazy val catsMacros        = "org.typelevel"              %% "cats-macros"         % catsMacrosVersion
lazy val catsEffect        = "org.typelevel"              %% "cats-effect"         % catsEffectVersion
lazy val circeCore         = "io.circe"                   %% "circe-core"          % circeVersion
lazy val circeGeneric      = "io.circe"                   %% "circe-generic"       % circeVersion
lazy val circeParser       = "io.circe"                   %% "circe-parser"        % circeVersion
lazy val commonsText       = "org.apache.commons"         %  "commons-text"        % commonsTextVersion
lazy val console4cats      = "dev.profunktor"             %% "console4cats"        % console4catsVersion
lazy val diffsonCirce      = "org.gnieh"                  %% "diffson-circe"       % diffsonVersion
// lazy val eff               = "org.atnos"                  %% "eff"                 % effVersion
lazy val fs2            = "co.fs2"            %% "fs2-core" % fs2Version
lazy val fs2io          = "co.fs2"            %% "fs2-io" % fs2Version
lazy val jgraphtCore    = "org.jgrapht"       % "jgrapht-core"     % jgraphtVersion
lazy val logbackClassic = "ch.qos.logback"    % "logback-classic"  % logbackVersion
lazy val jenaArq        = "org.apache.jena"   % "jena-arq"         % jenaVersion
lazy val jenaFuseki     = "org.apache.jena"   % "jena-fuseki-main" % jenaVersion
lazy val junit          = "junit"             % "junit"            % junitVersion
lazy val junitInterface = "com.novocode"      % "junit-interface"  % junitInterfaceVersion
lazy val rdf4j_runtime  = "org.eclipse.rdf4j" % "rdf4j-runtime"    % rdf4jVersion

// WESO components
lazy val document          = "es.weso"                    %% "document"        % documentVersion
lazy val srdf              = "es.weso"                    %% "srdf"            % srdfVersion
lazy val srdfJena          = "es.weso"                    %% "srdfjena"        % srdfVersion
lazy val srdf4j            = "es.weso"                    %% "srdf4j"          % srdfVersion
lazy val shapeMaps         = "es.weso"                    %% "shapemaps"       % shapeMapsVersion
lazy val utils             = "es.weso"                    %% "utils"           % utilsVersion
lazy val typing            = "es.weso"                    %% "typing"          % utilsVersion
lazy val validating        = "es.weso"                    %% "validating"      % utilsVersion
lazy val utilsTest         = "es.weso"                    %% "utilstest"       % utilsVersion

lazy val scalaLogging   = "com.typesafe.scala-logging" %% "scala-logging" % loggingVersion
lazy val scallop        = "org.rogach"                 %% "scallop"       % scallopVersion
lazy val scalactic      = "org.scalactic"              %% "scalactic"     % scalacticVersion
lazy val scalacheck     = "org.scalacheck"             %% "scalacheck"    % scalacheckVersion
lazy val scalaTest      = "org.scalatest"              %% "scalatest"     % scalaTestVersion
// lazy val scalatags      = "com.lihaoyi"                %% "scalatags"     % scalatagsVersion
lazy val sext           = "com.github.nikita-volkov"   % "sext"        % sextVersion
lazy val pprint         = "com.lihaoyi"                %% "pprint"     % pprintVersion
lazy val typesafeConfig = "com.typesafe"               % "config"      % typesafeConfigVersion
lazy val xercesImpl     = "xerces"                     % "xercesImpl"  % xercesVersion
lazy val simulacrum     = "org.typelevel"              %% "simulacrum" % simulacrumVersion

lazy val shexsRoot = project
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
  .disablePlugins(RevolverPlugin)
//  .settings(
//    buildInfoKeys := BuildInfoKey.ofN(name, version, scalaVersion, sbtVersion),
//    buildInfoPackage := "es.weso.shaclex.buildinfo"
//  )
  .settings(commonSettings, packagingSettings, publishSettings, ghPagesSettings, wixSettings)
  .aggregate(depGraphs, shex, shexTest, rbe, wikibaserdf)
  .dependsOn(depGraphs, shex, shexTest, rbe, wikibaserdf)
  .settings(
    siteSubdirName in ScalaUnidoc := "scaladoc/latest",
    addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(noDocProjects: _*),
    mappings in makeSite ++= Seq(
      file("src/assets/favicon.ico") -> "favicon.ico"
    ),
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      catsEffect,
      console4cats,
      logbackClassic,
      srdf,
      scalaLogging,
      scallop,
      typesafeConfig,
      pprint,
    ),
    cancelable in Global := true,
    fork := true,
    ThisBuild / turbo := true,
    crossScalaVersions := supportedScalaVersions,
    skip in publish := true,
    Compile / run / mainClass := Some("es.weso.shexs.Main")
  )

lazy val CompatTest                     = config("compat") extend (Test) describedAs ("Tests that check compatibility (some may fail)")
def compatFilter(name: String): Boolean = name endsWith "CompatTest"
def testFilter(name: String): Boolean   = /*(name endsWith "Test") && */ !compatFilter(name)

lazy val shex = project
  .in(file("modules/shex"))
  .enablePlugins(Antlr4Plugin)
  .disablePlugins(RevolverPlugin)
  .configs(CompatTest)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    commonSettings,
    publishSettings,
    antlrSettings("es.weso.shex.parser"),
    inConfig(CompatTest)(Defaults.testTasks),
    testOptions in Test := Seq(Tests.Filter(testFilter)),
    testOptions in CompatTest := Seq(Tests.Filter(compatFilter))
  )
  .dependsOn(
    rbe,
    depGraphs
  )
  .settings(
    libraryDependencies ++= Seq(
      typesafeConfig % Test,
      logbackClassic % Test,
      scalaLogging,
      circeCore,
      circeGeneric,
      circeParser,
      catsEffect,
      pprint,
      scalaTest  % Test,
      scalacheck % Test,
      typing,
      document,
      fs2, fs2io,
      utils     % "test -> test; compile -> compile",
      utilsTest % Test,
      validating,
      srdf,
      shapeMaps,
      srdfJena % Test,
      srdf4j   % Test,
      junit % Test,
      junitInterface % Test
    )
  )

lazy val depGraphs = project
  .in(file("modules/depGraphs"))
  .disablePlugins(RevolverPlugin)
  .settings(commonSettings, publishSettings)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      // catsMacros,
      jgraphtCore,
      utils
    )
  )

lazy val wikibaserdf = project
  .in(file("modules/wikibaserdf"))
  .disablePlugins(RevolverPlugin)
  .settings(commonSettings, publishSettings)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      // catsMacros,
      utils,
      srdf,
      srdfJena,
  )).dependsOn(
    shex
  )

lazy val shexTest = project
  .in(file("modules/shexTest"))
  .disablePlugins(RevolverPlugin)
  .configs(CompatTest)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    commonSettings,
    publishSettings,
    inConfig(CompatTest)(Defaults.testTasks),
    testOptions in Test := Seq(Tests.Filter(testFilter)),
    testOptions in CompatTest := Seq(Tests.Filter(compatFilter))
  )
  .dependsOn(
    shex
  )
  .settings(
    libraryDependencies ++= Seq(
      typesafeConfig % Test,
      logbackClassic % Test,
      scalaLogging,
      circeCore,
      circeGeneric,
      circeParser,
      scalaTest  % Test,
      scalacheck % Test,
      catsEffect,
      utils     % "test -> test; compile -> compile",
      utilsTest % Test,
      srdf,
      shapeMaps,
      srdfJena,
      srdf4j % Test
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
  .disablePlugins(RevolverPlugin)
  .dependsOn()
  .settings(
    commonSettings,
    publishSettings
  )
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      //    compilerPlugin(("org.typelevel" %% "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)),
      validating,
      typing,
      simulacrum,
      catsCore,
      catsKernel,
      // catsMacros,
      scalacheck % Test,
      srdfJena   % Test,
      utils,
      scalaLogging
    ) ++ macroDependencies(scalaVersion.value)
  )

/* ********************************************************
 ******************** Grouped Settings ********************
 **********************************************************/

lazy val noDocProjects = Seq[ProjectReference](
  )

lazy val noPublishSettings = Seq(
//  publish := (),
//  publishLocal := (),
  publishArtifact := false
)

lazy val sharedDependencies = Seq(
  libraryDependencies ++= Seq(
    scalactic,
    scalaTest % Test
  )
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
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.  "-encoding", "UTF-8",
    "-language:_",
    "-target:jvm-1.8",
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xlint",
    "-Yrangepos",
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    // "-Xfatal-warnings",
    "-Ywarn-extra-implicit"              // Warn when more than one implicit parameter section is defined.
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

lazy val ghPagesSettings = Seq(
  git.remoteRepo := "git@github.com:labra/shaclex.git"
)

lazy val commonSettings = compilationSettings ++ sharedDependencies ++ Seq(
  organization := "es.weso",
  resolvers ++= Seq(
    Resolver.bintrayRepo("labra", "maven"),
    Resolver.bintrayRepo("weso", "weso-releases"),
    Resolver.sonatypeRepo("snapshots")
  )
) ++ warnUnusedImport

def antlrSettings(packageName: String) = Seq(
  antlr4GenListener in Antlr4 := true,
  antlr4GenVisitor in Antlr4 := true,
  antlr4Dependency in Antlr4 := antlr4,
  antlr4PackageName in Antlr4 := Some(packageName)
)

lazy val publishSettings = Seq(
  maintainer := "Jose Emilio Labra Gayo <labra@uniovi.es>",
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
  scalacOptions in doc ++= Seq(
    "-diagrams-debug",
    "-doc-source-url",
    scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath",
    baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams"
  ),
  publishMavenStyle := true,
  bintrayRepository in bintray := "weso-releases",
  bintrayOrganization in bintray := Some("weso")
)

lazy val warnUnusedImport = Seq(
  scalacOptions ++= (if (isDotty.value) Nil else Seq("-Ywarn-unused:imports")),
  scalacOptions in (Compile, console) ~= { _.filterNot(Set("-Ywarn-unused-import", "-Ywarn-unused:imports")) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
)