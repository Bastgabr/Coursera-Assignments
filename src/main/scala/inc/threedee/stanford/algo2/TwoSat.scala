package inc.threedee.stanford.algo2

import io.Source
import util.Random

/**
 * Created by IntelliJ IDEA.
 * User: deeptia
 * Date: 26/01/13
 * Time: 10:10 PM
 * To change this template use File | Settings | File Templates.
 */
case class Clause(var1: Int, var2: Int) {
  def isClauseSatisfied(currentAssignment: Array[Boolean]): Boolean = {
    val var1CurrentAssignment: Boolean = currentAssignment(math.abs(var1) - 1)
    val var2CurrentAssignment: Boolean = currentAssignment(math.abs(var2) - 1)

    (isSatisfied(var1, var1CurrentAssignment) || isSatisfied(var2, var2CurrentAssignment))
  }

  private def isSatisfied(varValue: Int, varAssignment: Boolean): Boolean = {
    (((varValue < 0) && (varAssignment == false))
      ||
      ((varValue > 0) && (varAssignment == true)))
  }
}

object TwoSat {
  //Papadimitriou’s 2-SAT Algorithm
  def isSatisfiable(clauses: Array[Clause]): Boolean = {
    val numVariables: Int = clauses.size
    val twoNSquare: BigInt = BigInt(2) * BigInt(numVariables) * BigInt(numVariables)
    val logTwoN: Int = (math.log(numVariables) / math.log(2)).toInt
    //Repeat log(n, 2) times
    for (i <- 1 to logTwoN) {
      //Choose random initial assignment
      val currentAssignment: Array[Boolean] = Array.fill(numVariables) {
        Random.nextBoolean()
      }
      //Repeat 2*n^2 times
      var j: BigInt = 0
      while (j < twoNSquare) {
        currentAssignmentSatisfiesAllClauses(currentAssignment, clauses) match {
          case true => {
            //If current assignment satisfies all clauses, halt and report this
            return true
          }
          case false => {
            //Else, for each unsatisfied clause flip the value of one of its variables
            for (unsatisfiedClause <- clauses.filterNot(c => c.isClauseSatisfied(currentAssignment))) {
              //Choose between the two variables uniformly at random
              val chooseVar1: Boolean = Random.nextBoolean
              val varToBeFlipped: Int = chooseVar1 match {
                case true => unsatisfiedClause.var1
                case false => unsatisfiedClause.var2
              }
              val indexOfVarToBeFlipped: Int = math.abs(varToBeFlipped) - 1
              currentAssignment(indexOfVarToBeFlipped) = !currentAssignment(indexOfVarToBeFlipped)
            }
          }
        }
        j = j + 1
      }
    }
    false
  }

  private def currentAssignmentSatisfiesAllClauses(currentAssignment: Array[Boolean],
                                                   clauses: Array[Clause]): Boolean = {
    clauses.exists(!_.isClauseSatisfied(currentAssignment)) == false
  }

  private def parseInput(fileName: String): Array[Clause] = {
    val rowsFromFile: List[String] = List[String](Source.fromInputStream(getClass.getResourceAsStream(fileName))
      .getLines().toList: _*)
    val numVariables: Int = rowsFromFile.head.toInt
    val inputClauses: Array[Clause] = new Array[Clause](numVariables)
    var counter: Int = 0
    for (row <- rowsFromFile.tail) {
      val rowParts: Array[Int] = row.split(' ').map(_.toInt)
      inputClauses(counter) = Clause(rowParts(0), rowParts(1))
      counter = counter + 1
    }
    inputClauses
  }

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 6) {
      val inputFile: String = "/inc/threedee/stanford/algo2/2sat%d.txt".format(i)
      val inputClauses: Array[Clause] = parseInput(inputFile)
      println(isSatisfiable(inputClauses) match {
        case true => 1
        case false => 0
      })
    }
  }
}