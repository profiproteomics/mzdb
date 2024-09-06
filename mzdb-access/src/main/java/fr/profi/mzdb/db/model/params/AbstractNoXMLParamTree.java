package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.db.model.params.param.CVEntry;
import fr.profi.mzdb.db.model.params.param.CVParam;
import fr.profi.mzdb.db.model.params.param.UserParam;
import fr.profi.mzdb.db.model.params.param.UserText;

import java.util.ArrayList;
import java.util.List;

public class AbstractNoXMLParamTree extends AbstractParamTree {

    public List<CVParam> getCVParams() {
        if (this.cvParams == null)
            this.cvParams = new ArrayList<CVParam>();

        return cvParams;
    }

    // Marc: most of the object does not contain any UserParam,
    // so this is set to be non abstract to avoid to override it in subclasses
    // DBO: why ???
    public List<UserParam> getUserParams() {
        if (this.userParams == null)
            this.userParams = new ArrayList<UserParam>();

        return this.userParams;
    }

    public List<UserText> getUserTexts() {
        if (this.userTexts == null)
            this.userTexts = new ArrayList<UserText>();
        return this.userTexts;
    }

    public UserParam getUserParam(String name) {

        UserParam foundUP = null;
        for (UserParam up : this.getUserParams()) {
            if (up.getName().equals(name)) {
                foundUP = up;
                break;
            }
        }

        return foundUP;
    }

    public CVParam getCVParam(CVEntry cvEntry) {
        CVParam foundCV = null;
        for (CVParam cv : this.getCVParams()) {
            if (cv.getAccession().equals(cvEntry.getAccession()) ) {
                foundCV = cv;
                break;
            }
        }

        return foundCV;
    }

    public CVParam[] getCVParams(CVEntry[] cvEntries) {
        CVParam[] cvParams = new CVParam[cvEntries.length];

        int i = 0;
        for (CVEntry cvEntry : cvEntries) {
            cvParams[i] = this.getCVParam(cvEntry);
            i++;
        }

        return cvParams;
    }

}
