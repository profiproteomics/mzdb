package fr.profi.mzdb.io.reader.iterator;

import java.io.StreamCorruptedException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;

import fr.profi.mzdb.AbstractMzDbReader;
import fr.profi.mzdb.model.BoundingBox;
import fr.profi.mzdb.model.Spectrum;
import fr.profi.mzdb.model.SpectrumSlice;
import fr.profi.mzdb.util.sqlite.ISQLiteStatementConsumer;


public class SpectrumIterator extends AbstractSpectrumSliceIterator implements Iterator<Spectrum> {

	private static final String allMsLevelsSqlQuery = "SELECT bounding_box.* FROM bounding_box, spectrum WHERE spectrum.id = bounding_box.first_spectrum_id";
	private static final String singleMsLevelSqlQuery = allMsLevelsSqlQuery + " AND spectrum.ms_level= ?";

	protected int spectrumSliceIdx;

	protected SpectrumSlice[] firstSpectrumSlices = null;
	protected SpectrumSlice[] spectrumSliceBuffer = null;
	protected boolean bbHasNext = true;
	
	public SpectrumIterator(AbstractMzDbReader mzDbReader, SQLiteConnection connection) throws SQLiteException, StreamCorruptedException {
		super(mzDbReader.getSpectrumHeaderReader(), mzDbReader.getDataEncodingReader(), connection, allMsLevelsSqlQuery);

		this.initSpectrumSliceBuffer();
	}

	public SpectrumIterator(AbstractMzDbReader mzDbReader, SQLiteConnection connection, final int msLevel) throws SQLiteException, StreamCorruptedException {
		//super(inst, sqlQuery, msLevel, rethrowConsumer( (stmt) -> stmt.bind(1, msLevel) ) ); // Bind msLevel
		super(
			mzDbReader.getSpectrumHeaderReader(),
			mzDbReader.getDataEncodingReader(),
			connection,
			singleMsLevelSqlQuery,
			msLevel,
			(ISQLiteStatementConsumer) stmt -> {
				stmt.bind(1, msLevel); // Bind msLevel
			}
    );

		this.initSpectrumSliceBuffer();
	}

	protected void initSpectrumSliceBuffer() {

		if (this.firstSpectrumSlices != null) {
			this.spectrumSliceBuffer = this.firstSpectrumSlices;
		} else {
			this.spectrumSliceBuffer = this.firstBB.toSpectrumSlices();
		}

		this.spectrumSliceIdx = 0;
		//Use List instead of Array: firstBB may not contain all spectra ids ->  have to complete list
		List<SpectrumSlice> spectrumSliceList = new ArrayList<>(List.of(this.spectrumSliceBuffer));
		spectrumSliceList.sort(Comparator.comparingLong(SpectrumSlice::getSpectrumId));

		// Build spectrum slice buffer
		while (bbHasNext = boundingBoxIterator.hasNext()) {

			BoundingBox bb = boundingBoxIterator.next();
			SpectrumSlice[] sSlices = bb.toSpectrumSlices();

			if (sSlices == null)
				continue;

			//Test same BB.FirstSpectrumId (getSpectrumId may not be the same as sSlices[0].getSpectrumId() if no data for first spectrum in first BB)
			if (sSlices[0].getHeader().getBBFirstSpectrumId() == spectrumSliceBuffer[0].getHeader().getBBFirstSpectrumId()) {
        for (SpectrumSlice sSlice : sSlices) {
          long sSpectrumId = sSlice.getSpectrumId();

          //Should verify spectrumIds. Not all have data
          int indexToInsert = -1;
          for (int j = 0; j < spectrumSliceList.size(); j++) {
            SpectrumSlice nextSSlice = spectrumSliceList.get(j);
            long nextSpSliceId = nextSSlice.getSpectrumId();

            if (nextSpSliceId == sSpectrumId) {
              nextSSlice.getData().addSpectrumData(sSlice.getData());
              break;
            } else if (nextSpSliceId > sSpectrumId) {//does not exist yet, insert in List
              indexToInsert = j;
              break;
            }
          }
          if (indexToInsert != -1) {
            //insert sSlice in final list
            spectrumSliceList.add(indexToInsert, sSlice);
          }
        }
			} else {
				// Keep this bounding box for next iteration
				this.firstBB = bb;
				this.firstSpectrumSlices = sSlices;

				break;
			}
		}
		spectrumSliceBuffer = spectrumSliceList.toArray(SpectrumSlice[]::new);
	}


	public Spectrum next() {

		// firstSpectrumSlices is not null
		int spectrumSliceIdxCopy = spectrumSliceIdx;
		spectrumSliceIdx++;

		Spectrum spectrum = spectrumSliceBuffer[spectrumSliceIdxCopy];
		boolean noMoreSpectrum = spectrumSliceIdx == spectrumSliceBuffer.length;

		// If no more spectrum slices
		if (noMoreSpectrum) {
			if (bbHasNext)
				initSpectrumSliceBuffer();
			else
				this.firstBB = null;
		}

		return spectrum;
	}

}