package fr.profi.mzdb.utils

import java.util.concurrent.atomic.AtomicInteger

package object misc {

  trait InMemoryIdGen {
    private val inMemoryIdSequence = new AtomicInteger(0)
    
    def generateNewId(): Int = inMemoryIdSequence.incrementAndGet
  }
  
  /*trait InMemoryIdGen extends AbstractInMemoryIdGen {    
    def generateNewId() : Int = { this.generateNewId() }
  }*/
   
}

