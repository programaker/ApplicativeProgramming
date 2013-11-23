package domain

case class Person(name: String, age: Int, postcode: String)

object Person {
  def validName(nameInput: String): Validation[List[String], String] = {
    if(nameInput.headOption.exists(_.isUpper)) {
      Success(nameInput)
    } else {
      Failure(List("Name must begin with capital letter"))
    }
  }

  def validAge(ageInput: String): Validation[List[String], Int] = {
    try {
      val age = ageInput.toInt

      if(age < 0) {
        Failure(List("Age must be greater than 0"))
      } else if(age > 130) {
        Failure(List("Age must be less than 130"))
      } else {
        Success(age)
      }
    } catch {
      case e: Exception => Failure(List(s"$e"))
    }
  }

  def validPostcode(postcodeInput: String): Validation[List[String], String] = {
    if(postcodeInput.length == 4 && postcodeInput.forall(_.isDigit)) {
      Success(postcodeInput)
    } else {
      Failure(List("Postcode must be 4 digits"))
    }
  }
}