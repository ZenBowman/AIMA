import org.zenbowman.logic.propositional.{Resolution, PropositionalLogic, KnowledgeBase}


import PropositionalLogic._

val kb = new KnowledgeBase

kb.tell('a or 'b)
kb.tell(not('a))
kb.tell('b -> 'c)
kb.tell('a <-> 'd)




kb.ask('b)

































