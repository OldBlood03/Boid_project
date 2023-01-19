package s2.coffee

import scala.util.Random

class Coffeemaker:

  val maxBeanAmount = 100
  val maxCleanliness = 15
  var coffeeBeanAmount = maxBeanAmount
  var cleanliness = maxCleanliness

  /** Prepares a cup of coffee. Throws NoBeansException if the machine runs out coffee beans. Throws
    * MachineDirtyException if the machine is used many times and it becomes dirty.
    */
  def makeCoffee(): Coffee =
    val coffee = Coffee()
    if coffeeBeanAmount - coffee.beanConsumption < 0 then throw NoBeansException("Not enough coffee beans.")
    else if cleanliness <= 0 then throw MachineDirtyException("Coffeemaker needs cleaning.")

    cleanliness -= 1
    coffeeBeanAmount -= coffee.beanConsumption
    coffee
  end makeCoffee

  /** Adds milk to the coffee Throws MilkSpilledexception if the coffee is spilled while adding milk.
    */
  def addMilk(coffee: Coffee): Coffee =
    val spilled = Random.nextBoolean()
    if spilled then throw MilkSpilledException("You added too much milk.")
    coffee.milk += 2
    coffee
end Coffeemaker
