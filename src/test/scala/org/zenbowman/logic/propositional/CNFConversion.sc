import org.zenbowman.logic.propositional.{PropositionalLogic, KnowledgeBase}
import PropositionalLogic._

val kb = new KnowledgeBase

kb.tell('breeze_1_1 <-> ('pit_1_2 or 'pit_2_1))







kb.dump()



for (clause <- kb.as3CNF) {
  println(clause)
}








































