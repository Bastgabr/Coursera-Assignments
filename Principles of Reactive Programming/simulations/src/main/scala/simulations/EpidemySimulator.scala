package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val incubationTime = 6
    val dieTime = 14
    val immuneTime = 16
    val healTime = 18

    val prevalenceRate = 0.01
    val transRate = 0.4
    val dieRate = 0.25

    val moveMaxDelayDays = 5
  }

  import SimConfig._

  val initiallyInfectedPersonIds: Set[Int] = Set[Int](randomBelow(population),
    randomBelow(population), randomBelow(population))
  val persons: List[Person] = (1 to population).toList.map(id => {
    val person = new Person(id)
    if (initiallyInfectedPersonIds.contains(id)) {
      person.infected = true
    }
    person
  })

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def move(): Unit = {
      val rowDiff = randomBelow(2)
      val colDiff = randomBelow(2)

      val nextRow = rowDiff match {
        case 0 => (row - rowDiff) % roomRows
        case 1 => (row + rowDiff) % roomRows
      }

      val nextColumn = colDiff match {
        case 0 => (col - colDiff) % roomColumns
        case 1 => (col + colDiff) % roomColumns
      }
      val personsAtNextPosition: List[Person] = persons.filter(p => p.row == nextRow && p.col == nextColumn)
      val canMove: Boolean = {
        !this.dead &&
          personsAtNextPosition.find(p => { p.sick || p.dead }) == None
      }

      if (canMove) {
        this.row = nextRow
        this.col = nextColumn
      }
      if (!infected && !sick && !immune && !dead && personsAtNextPosition.find(_.infected) != None) {
        this.infected = randomBelow(100) < (transRate * 100)
      }

      afterDelay(incubationTime)({
        if (infected && !sick && !immune && !dead) {
          this.sick = true
        }
      })

      afterDelay(dieTime)({
        if (infected && !immune && !dead && randomBelow(100) < (dieRate * 100)) {
          this.dead = true
        }
      })

      afterDelay(immuneTime)({
        if (infected && !dead) {
          this.immune = true
          this.sick = false
        }
      })

      afterDelay(healTime)({
        if (infected && !dead) {
          this.infected = false
          this.sick = false
          this.immune = false
        }
      })
    }
  }

  persons.foreach(person => {
    afterDelay(0)(person.move)
    afterDelay(randomBelow(moveMaxDelayDays - 1) + 1)(person.move)
  })
}
