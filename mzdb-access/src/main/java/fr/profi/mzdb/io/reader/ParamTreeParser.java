package fr.profi.mzdb.io.reader;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;

import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.db.model.params.InstrumentConfigParamTree;
import fr.profi.mzdb.db.model.params.ParamTree;

// TODO: Auto-generated Javadoc
/**
 * The Class ParamTreeParser.
 *
 * @author David Bouyssie
 */
public class ParamTreeParser {

  /**
   * Parses the param tree.
   *
   * @param paramTreeAsStr the param tree as str
   * @return the param tree
   */
  public static ParamTree parseParamTree( String paramTreeAsStr ) {
    
    ParamTree paramTree = null;
    try {    
      paramTree = MzDbReader.xmlMapper.readValue(paramTreeAsStr, ParamTree.class);
    } catch (JsonParseException e) {
      e.printStackTrace();
    } catch (JsonMappingException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    }
    
    return paramTree;    
  }
  
  /**
   * Parses the instrument config param tree.
   *
   * @param paramTreeAsStr the param tree as str
   * @return the instrument config param tree
   */
  public static InstrumentConfigParamTree parseInstrumentConfigParamTree( String paramTreeAsStr ) {
    
    InstrumentConfigParamTree paramTree = null;
    try {    
      paramTree = MzDbReader.xmlMapper.readValue(paramTreeAsStr, InstrumentConfigParamTree.class);
    } catch (JsonParseException e) {
      e.printStackTrace();
    } catch (JsonMappingException e) {
      e.printStackTrace();
    } catch (IOException e) {
      e.printStackTrace();
    }
    
    return paramTree;    
  }

  
}
