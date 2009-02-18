package de.istjascha.util

import collection.mutable.HashSet
class LowerCaseHashSetString extends HashSet[String]{

  override def +=(elem: String) { addEntry(elem.toLowerCase) }
 
  override def -=(elem: String) { removeEntry(elem.toLowerCase) }

}
