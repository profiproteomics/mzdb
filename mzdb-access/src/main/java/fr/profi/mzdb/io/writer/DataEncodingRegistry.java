package fr.profi.mzdb.io.writer;

import fr.profi.mzdb.model.DataEncoding;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

class DataEncodingRegistry {
  HashMap<String, DataEncoding> dataEncodingMap = new HashMap();
  private int id = 1;

  public DataEncoding getOrAddDataEncoding(DataEncoding de){
    String key = de.getMode().name()+de.getPeakEncoding().name();
    if(dataEncodingMap.containsKey(key))
      return dataEncodingMap.get(key);
    else {
      DataEncoding newDe = new DataEncoding(id++, de.getMode(), de.getPeakEncoding(), de.getCompression(),de.getByteOrder());
      dataEncodingMap.put(key,newDe);
      return newDe;
    }
  }

  public List<DataEncoding> getDistinctDataEncoding() {
     List<DataEncoding> distinctDE = new ArrayList<>();
     distinctDE.addAll(dataEncodingMap.values());
     distinctDE.sort((o1, o2) -> {
       if(o1 == null)
         return -1;
       if(o2 == null)
         return 1;
       if(o1.getId() < o2.getId())
         return -1;
       if(o1.getId() == o2.getId())
         return 0;
       else
         return 1;
     });

     return  distinctDE;
  }
}
