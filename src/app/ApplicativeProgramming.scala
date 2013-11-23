package app

import domain.{Failure, Success, Person}

// Applicative Programming, Disjoint Unions, Semigroups and Non-breaking Error Handling
object ApplicativeProgramming {
  def main(args: Array[String]) = {
    if(args.length < 3) {
      println("Needs 3 arguments: name, age and postcode")
    } else {
      val nameParam = args(0)
      val ageParam = args(1)
      val postcodeParam = args(2)
      run(nameParam, ageParam, postcodeParam)
    }
  }

  def run(nameParam: String, ageParam: String, postcodeParam: String) = {
    val personConstructor = (Person(_, _, _)).curried
    val name = Person.validName(nameParam)
    val age = Person.validAge(ageParam)
    val postcode = Person.validPostcode(postcodeParam)

    postcode.apply(age.apply(name.map(personConstructor))) match {
      case Success(newPerson) => println(s"created $newPerson")
      case Failure(failures) => failures foreach println
    }
  }
}
