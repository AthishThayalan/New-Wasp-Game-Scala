case class DroneWasp(val hp: Int = 60) extends Wasp("Drone Wasp", hp, 12) {
  override def createWasp(healthPoints: Int): Wasp = new DroneWasp(healthPoints)
}