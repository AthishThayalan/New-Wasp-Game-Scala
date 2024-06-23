case class QueenWasp(val hp: Int = 80) extends Wasp("Queen", hp, 7) {
  override def createWasp(healthPoints: Int): Wasp = new QueenWasp(healthPoints)
}