package fr.profi.mzdb.util

/**
 * @author David Bouyssie
 *
 */
package object sqlite {

  class RichSQLiteQuery(self: SQLiteQuery) {
    def eachRecord( fn: SQLiteRecord => Unit ): Unit = {
      self.forEachRecord( new Object with ISQLiteRecordOperation {
        def execute( p: SQLiteRecord, idx: Int ) = fn(p)
      } )
    }
    def eachRecord( fn: (SQLiteRecord, Int) => Unit ): Unit = {
      self.forEachRecord( new Object with ISQLiteRecordOperation {
        def execute( p: SQLiteRecord, idx: Int ) = fn(p, idx)
      } )
    }
  }
  
  implicit def queryToRichQuery(query: SQLiteQuery) = new RichSQLiteQuery(query)
  
    /*object OnEachRecord {
      def execute( p: SQLiteRecord ) = {}
      def apply(fn: SQLiteRecord => Unit) = {
        new Object with ISQLiteStmtOperation {
          def execute( p: SQLiteRecord ) = fn
        }
      }
    }*/
}