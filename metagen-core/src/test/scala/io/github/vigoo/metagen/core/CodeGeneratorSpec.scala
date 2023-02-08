package io.github.vigoo.metagen.core

import zio.nio.file.Files
import zio.test.TestAspect.{diagnose, timeout}
import zio.{Scope, ZIO, durationInt}
import zio.test.{Spec, TestEnvironment, ZIOSpecDefault, assertCompletes, assertTrue}

import java.nio.charset.StandardCharsets
import scala.meta._

object CodeGeneratorSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("metagen code generator")(
      test("should be able to inject scaladoc to classes and methods") {
        val gen: ZIO[CodeFileGenerator, Nothing, Term.Block] =
          for {
            doc1 <- CodeFileGenerator.addScaladoc("Some documentation\n\nwith multiple lines")
            doc2 <- CodeFileGenerator.addScaladoc("Single line one")
            doc3 <- CodeFileGenerator.addScaladoc("Another one\nwith\nmultiple\nlines")
          } yield q"""
            $doc1 case class Test() {
              $doc2 def testMethod(
                param1: String,
                param2: String
              ): Unit = ()
            }

            object Test {
              $doc3 def anotherMethod: Unit = ()
            }
          """

        for {
          tempDir        <- Files.createTempDirectoryScoped(None, Iterable.empty)
          _              <- Generator.setScalaVersion("2.13.10")
          _              <- Generator.enableFormatting()
          _              <- Generator.setRoot(tempDir)
          _              <- Generator.generateScalaPackage[Any, Nothing](Package("test"), "Test")(gen)
          generatedBytes <- Files.readAllBytes(tempDir / "test" / "Test.scala")
          generated       = new String(generatedBytes.toArray, StandardCharsets.UTF_8)
        } yield assertTrue(
          generated ==
            """package test
              |
              |/** Some documentation
              |  *
              |  * with multiple lines
              |  */
              |case class Test() {
              |
              |  /** Single line one */
              |  def testMethod(param1: String, param2: String): Unit = ()
              |}
              |object Test {
              |
              |  /** Another one with multiple lines
              |    */
              |  def anotherMethod: Unit = ()
              |}
              |""".stripMargin
        )
      }
    ).provideSome[Scope](
      Generator.live
    )
}
