package app

import domain.{Validation, Failure, Success, Person}

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
    val personConstructor: String => Int => String => Person = (Person(_, _, _)).curried
    val name: Validation[List[String], String] = Person.validName(nameParam)
    val age: Validation[List[String], Int] = Person.validAge(ageParam)
    val postcode: Validation[List[String], String] = Person.validPostcode(postcodeParam)

    postcode.apply(age.apply(name.map(personConstructor))) match {
      case Success(newPerson) => println(s"created $newPerson")
      case Failure(failures) => failures foreach println
    }
  }
}
