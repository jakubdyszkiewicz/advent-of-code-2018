package day14

object ChocolateCharts {

  def makeRecipe(recipes: Vector[Int], elvesIdxs: Seq[Int]): (Vector[Int], Seq[Int]) = {
    val recipeCombined = elvesIdxs.map(idx => recipes(idx)).sum
    val recipe1 = recipeCombined / 10
    val recipe2 = recipeCombined % 10
    var newRecipies = recipes
    if (recipe1 != 0) {
      newRecipies = newRecipies :+ recipe1
    }
    newRecipies = newRecipies :+ recipe2

    val newIdxs = elvesIdxs.map(idx => (idx + 1 + newRecipies(idx)) % newRecipies.length)
    (newRecipies, newIdxs)
  }

  def makeRecipesUntilScore(
    score: Int,
    recipes: Vector[Int] = Vector(3, 7),
    elvesIdxs: Seq[Int] = Vector(0, 1))
  : Seq[Int] = {
    if (recipes.length >= score + 10) {
      return recipes.slice(score, score + 10)
    }
    val (newRecipes, newElvesIdx) = makeRecipe(recipes, elvesIdxs)
    makeRecipesUntilScore(score, newRecipes, newElvesIdx)
  }

  // returns how many recipes was made before sequence
  def makeRecipesUntilSequence(
    sequence: String,
    recipes: Vector[Int] = Vector(3, 7),
    elvesIdxs: Seq[Int] = Vector(0, 1))
  : Int = {
    val lastRecipes = recipes.takeRight(sequence.length + 1).foldLeft("") { _ + _ }
    val seqStartIdx = lastRecipes.indexOf(sequence)
    if (seqStartIdx != -1) {
      return recipes.length - lastRecipes.length + seqStartIdx
    }
    val (newRecipes, newElvesIdx) = makeRecipe(recipes, elvesIdxs)
    makeRecipesUntilSequence(sequence, newRecipes, newElvesIdx)
  }

  def main(args: Array[String]): Unit = {
    val result = makeRecipesUntilScore(633601)
    println(result.foldLeft("") { _ + _ })

    val part2 = makeRecipesUntilSequence("633601")
    println(part2)
  }
}
