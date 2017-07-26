sealed trait Id
class A extends Id {}
class B extends Id {}

sealed trait Container[-Id]
class AC extends Container[A]
class BC extends Container[B]

def check(container: Container[A]) = println(container.getClass)

check(new AC)

