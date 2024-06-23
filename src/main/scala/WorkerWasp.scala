case class WorkerWasp( val hp: Int = 68) extends Wasp("Worker", hp, 10) {
  override def createWasp(healthPoints: Int): Wasp = new WorkerWasp(healthPoints)
}