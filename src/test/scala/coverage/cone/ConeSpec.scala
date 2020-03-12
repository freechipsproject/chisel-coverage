package coverage.cone

import firrtl.analyses.{CircuitGraph, ConnectionGraph}
import firrtl.{ChirrtlForm, CircuitState, LowFirrtlCompiler, LowForm}
import firrtl.ir.Circuit
import firrtl.stage.FirrtlStage
import firrtlTests.FirrtlFlatSpec
import org.scalatest.FlatSpec
import coverage.TestUtils._

object ConeSpec {
  val circuitString =
    s"""circuit Top:
       |  module Top:
       |    input in: UInt<8>[4]
       |    input clock: Clock
       |    input reset: UInt<1>
       |    output out: UInt<8>[5]
       |    out[0] <= add(in[0], in[1])
       |    out[1] <= add(in[2], in[3])
       |    out[2] is invalid
       |    reg r: UInt<8>, clock with: (reset => (reset, in[0]))
       |    r <= in[3]
       |    out[3] <= r
       |    node x = add(in[0], UInt(0))
       |    out[4] <= x
     """.stripMargin

  def lower(circuit: Circuit): Circuit = {
    val xForms = firrtl.CompilerUtils.getLoweringTransforms(ChirrtlForm, LowForm)
    val res = xForms.foldLeft(CircuitState(circuit, ChirrtlForm)) { (cs, xform) => xform.runTransform(cs) }
    res.circuit
  }
}

class ConeSpec extends FirrtlFlatSpec {
  import ConeSpec._

  val circuit = toMiddleFIRRTL(parse(circuitString))
  val coneGraph = ConeFinder(ConnectionGraph(circuit).reverseConnectionGraph)

  "Identical add cones" should "match" in {
    val out0Cone = coneGraph.getCone("~Top|Top>out[0]".RT, Seq("~Top|Top>in[0]".RT, "~Top|Top>in[1]".RT))
    val out1Cone = coneGraph.getCone("~Top|Top>out[1]".RT, Seq("~Top|Top>in[2]".RT, "~Top|Top>in[3]".RT))
    out0Cone should be(out1Cone)
    out0Cone should be(Cone(Seq(
      Assign("add", Seq(1, 2), Nil),
      Terminal(8, "UInt"),
      Terminal(8, "UInt")
    )))
  }

  "Invalidated signals" should "match" in {
    val a = coneGraph.getCone("~Top|Top>out[2]".RT, Nil)
    a should be(Cone(Seq(Invalid(BigInt(8), "UInt"))))
  }

  "Registered signals" should "unique" in {
    val r = coneGraph.getCone("~Top|Top>out[3]".RT,Seq(
      "~Top|Top>clock".RT,
      "~Top|Top>reset".RT,
      "~Top|Top>in[0]".RT,
      "~Top|Top>in[3]".RT
    ))
    r should be(Cone(Seq(
      Assign("reg", List(1, 2, 3, 4), Nil),
      Terminal(1, "Clock"),
      Terminal(1, "UInt"),
      Terminal(8, "UInt"),
      Terminal(8, "UInt")
    )))
  }

  "Literals" should "work properly" in {
    val r = coneGraph.getCone("~Top|Top>out[4]".RT,Seq(
      "~Top|Top>in[0]".RT
    ))
    r should be(Cone(Seq(
      Assign("add", List(1, 2), Nil),
      Terminal(8, "UInt"),
      Literal(1, "UInt", 0)
    )))
  }

  "Memories" should "work properly" ignore {
    sys.error("Not Implemented")
  }
}
