package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")

    in2.setSignal(false)
    run

    assert(out.getSignal === true, "or 4")

    in1.setSignal(false)
    run

    assert(out.getSignal === false, "or 5")
  }

  test("demux1") {
    val in, c0, o0, o1 = new Wire
    demux(in, List(c0), List(o1, o0))

    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)

    in.setSignal(true)
    run
    assert(o0.getSignal === true, 0)
    assert(o1.getSignal === false, 1)

    c0.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === true, 1)
  }

  test("demux2") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    demux(in, List(c1, c0), List(o0, o1, o2, o3))

    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)

    in.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === true, 3)

    c0.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === true, 2)
    assert(o3.getSignal === false, 3)

    c0.setSignal(false)
    c1.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === true, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)

    c0.setSignal(true)
    run
    assert(o0.getSignal === true, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
  }

  test("demux 1-3-8") {
    implicit def boolean2Int(b: Boolean) = if (b) 1 else 0
    implicit def int2boolean(bit: Int) = if (bit > 0) true else false

    def setSignals(wires: List[Wire], signals: Int*) {
      (wires zip signals) foreach { case (wire, signal) => wire setSignal signal }
    }
    val in = new Wire
    val control = List.fill(3)(new Wire)
    val out = List.fill(8)(new Wire)
    demux(in, control, out)
    in.setSignal(1)

    setSignals(control, 0, 0, 0); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 0, 0, 0, 0, 0, 1))

    setSignals(control, 0, 0, 1); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 0, 0, 0, 0, 1, 0))

    setSignals(control, 0, 1, 0); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 0, 0, 0, 1, 0, 0))

    setSignals(control, 0, 1, 1); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 0, 0, 1, 0, 0, 0))

    setSignals(control, 1, 0, 0); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 0, 1, 0, 0, 0, 0))

    setSignals(control, 1, 0, 1); run
    assert(out.map(_.getSignal.toInt) === List(0, 0, 1, 0, 0, 0, 0, 0))

    setSignals(control, 1, 1, 0); run
    assert(out.map(_.getSignal.toInt) === List(0, 1, 0, 0, 0, 0, 0, 0))

    setSignals(control, 1, 1, 1); run
    assert(out.map(_.getSignal.toInt) === List(1, 0, 0, 0, 0, 0, 0, 0))
  }
}
