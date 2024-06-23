import scala.annotation.tailrec
import scala.util.Random
import scala.io.StdIn.readLine

object WaspGame {

  def initialise(): List[Wasp] = {
    new QueenWasp() :: List.fill(5)(new WorkerWasp()) ::: List.fill(8)(new DroneWasp())
  }

  def hitRandomWasp(wasps: List[Wasp]): List[Wasp] = {
    val aliveWasps = wasps.filter(_.isAlive)
    if (aliveWasps.isEmpty) {
      wasps
    } else {
      val randomIndex = Random.nextInt(aliveWasps.length)
      val randomWasp = aliveWasps(randomIndex)
      val updatedRandomWasp = randomWasp.hit()
      println(s"Random wasp: $updatedRandomWasp")
      val updatedWasps = wasps.map { wasp =>
        if (wasp eq randomWasp) updatedRandomWasp else wasp
      }
      if (winCondition(updatedWasps)) {
        killAllWasps(updatedWasps)
      } else {
        updatedWasps
      }
    }
  }

  private def hitQueen(wasps: List[Wasp]): List[Wasp] = {
    val updatedWasps = wasps.map {
      case queen: QueenWasp => queen.hit()
      case wasp => wasp
    }

    updatedWasps
  }

  private def killAllWasps(wasps: List[Wasp]): List[Wasp] = {
    wasps.map(wasp => wasp.createWasp(0))
  }

  @tailrec
  private def fireUntilQueenDies(wasps: List[Wasp]): Unit = {
    if (winCondition(wasps)) {
      println("The Queen Wasp is dead. You win.")
      askForRestart()
    } else {
      val updatedWasps = hitRandomWasp(wasps)
      displayState(updatedWasps)
      fireUntilQueenDies(updatedWasps)
    }

  }

  def displayState(wasps: List[Wasp]): Unit = {
    println("Current Wasp States:")
    println("====================")
    wasps.foreach { wasp =>
      val status = if (wasp.isAlive) "Alive" else "Dead"
      println(f"${wasp.name}%-20s ${wasp.healthPoints} HP [$status]")
    }
    println("====================\n")
  }


  def winCondition(wasp: List[Wasp]): Boolean = !wasp.filter(_.isAlive).exists(_.isInstanceOf[QueenWasp])

  @tailrec
  private def askForRestart(): Unit = {
    println("Do you want to restart the game? (yes/no): ")
    val response = readLine().trim.toLowerCase
    response match {
      case "yes" => gameLoop(initialise())
      case "no" => println("Exiting game ...")
      case _ =>
        println("Invalid response. Please enter yes or no.")
        askForRestart()
    }
  }

  @tailrec
  private def gameLoop(wasps: List[Wasp]): Unit = {
    displayState(wasps)
    println("Enter Commands (fire,restart,quit): ")
    val command = readLine().trim.toLowerCase

    command match {
      case "fire" =>
        val updatedWasps = hitRandomWasp(wasps)
        if (winCondition(updatedWasps)) {
          println("The Queen Wasp and her hive are dead! You win.")
          askForRestart()
        } else gameLoop(updatedWasps)
      case "restart" => gameLoop(initialise())
      case "q" =>
        val updatedWasps2 = hitQueen(wasps)
        if (winCondition(updatedWasps2)) {
          println("The Queen Wasp and her hive are dead! You win.")
          askForRestart()
        } else gameLoop(updatedWasps2)
      case "fireuntil" =>
        fireUntilQueenDies(wasps)
      case "quit" => println("Exiting game ... ")
      case _ =>
        println("Invalid command. Please enter fire, restart, quit, or fireuntil.")
        gameLoop(wasps)
    }


  }

  def main(args: Array[String]): Unit = {
    gameLoop(initialise())
  }


}
