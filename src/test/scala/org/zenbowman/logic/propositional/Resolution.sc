import org.zenbowman.logic.propositional.{Resolution, PropositionalLogic, KnowledgeBase}

import PropositionalLogic._

val dj1 = ExpandedDisjunction(List(not('breeze_1_1), 'breeze_1_2))

val lit1 = 'breeze_1_1
println(dj1)

val dj2 = Resolution.unitResolve(dj1, lit1)

println(dj2)

