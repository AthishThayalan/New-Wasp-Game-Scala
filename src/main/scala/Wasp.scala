abstract class Wasp(val name: String, val healthPoints: Int, val damagePerHit: Int) extends WaspBehaviour {

  def hit(): Wasp = {
    val newHealthPoints = math.max(healthPoints - damagePerHit, 0)
    createWasp(newHealthPoints)
  }

  def isAlive: Boolean = healthPoints > 0

  def createWasp(healthPoints: Int): Wasp

  override def toString: String = s"$name: $healthPoints HP"
}