package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () =>
        afterDelay(0) {
          println(
            "  " + currentTime + ": " + name + " -> " + wire.getSignal)
        }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val notA1, notA2, notOutput = new Wire
    inverter(a1, notA1)
    inverter(a2, notA2)
    andGate(notA1, notA2, notOutput)
    inverter(notOutput, output)
  }

  def identityGate(a1: Wire, output: Wire) {
    def noOpAction() {
      val a1Sig = a1.getSignal
      afterDelay(0) { output.setSignal(a1Sig) }
    }
    a1 addAction noOpAction
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def demuxInternal(inputs: List[Wire], controls: List[Wire]): List[Wire] = {
      controls match {
        case headControl :: restControl => {
          val headControlNot = new Wire
          inverter(headControl, headControlNot)
          val outputs = inputs.map(input => {
            val o1 = new Wire
            val o2 = new Wire
            andGate(input, headControl, o1)
            andGate(input, headControlNot, o2)
            List(o1, o2)
          }).flatten
          demuxInternal(outputs, restControl)
        }
        case Nil => {
          inputs
        }
      }
    }
    val outputs = demuxInternal(List(in), c)
    for (i <- 0 until outputs.length) {
      identityGate(outputs(i), out(i))
    }
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
