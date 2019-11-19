package fr.profi.mzdb.util

import java.util.concurrent.atomic.AtomicInteger

package misc {

  trait InMemoryIdGen {
    private val inMemoryIdSequence = new AtomicInteger(0)
    
    def generateNewId(): Int = inMemoryIdSequence.incrementAndGet
  }
  
  /*trait InMemoryIdGen extends AbstractInMemoryIdGen {    
    def generateNewId() : Int = { this.generateNewId() }
  }*/
   
}

