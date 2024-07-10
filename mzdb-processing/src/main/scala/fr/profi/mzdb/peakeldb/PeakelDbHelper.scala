package fr.profi.mzdb.peakeldb

import com.almworks.sqlite4java.SQLiteConnection
import com.github.davidmoten.rtree.{RTree, geometry}
import fr.profi.chemistry.model.MolecularConstants
import fr.profi.ms.algo.IsotopePatternEstimator
import fr.profi.mzdb.Settings
import fr.profi.mzdb.algo.PeakelsPatternPredictor
import fr.profi.mzdb.model.{Peakel, PutativeFeature}
import fr.profi.mzdb.peakeldb.io.PeakelDbReader
import fr.profi.mzdb.util.ms.MsUtils
import fr.profi.util.metrics.Metric
import org.apache.commons.math3.stat.correlation.PearsonsCorrelation

import scala.collection.mutable.{ArrayBuffer, HashSet}
import scala.util.control.Breaks.{break, breakable}

object PeakelDbHelper {

  // Important: SQlite R*Tree floating values are 32bits floats, thus we need to expand the search
  // Advice from SQLite developers (https://www.sqlite.org/rtree.html#roundoff_error):
  // Applications should expand their contained-within query boxes slightly (by 0.000012%)
  // by rounding down the lower coordinates and rounding up the top coordinates, in each dimension.
  private val SQLITE_RTREE_UB_CORR = 1.0 + 0.00000012
  private val SQLITE_RTREE_LB_CORR = 1.0 - 0.00000012

  private val MIN_CORRELATION_THRESHOLD = Settings.isotopicPeakelsCorrelationThreshold
  private val PEARSON = new PearsonsCorrelation()
  

  def findPeakelIdsInRange(
    rTree: RTree[java.lang.Integer, geometry.Point],
    minMz: Double,
    maxMz: Double,
    minTime: Float,
    maxTime: Float): Seq[Int] = {

    // Retrieve peakel ids using the in-memory R*Tree
    val peakelIdIter = rTree.search(
      geometry.Geometries.rectangle(
        minMz, minTime, maxMz, maxTime
      )
    ).toBlocking().toIterable().iterator()

    val peakelIds = new ArrayBuffer[Int]()
    while (peakelIdIter.hasNext()) {
      peakelIds += peakelIdIter.next().value()
    }

    peakelIds
  }

  def findPeakelsInRange(
    sqliteConn: SQLiteConnection,
    rTreeOpt: Option[RTree[java.lang.Integer, geometry.Point]],
    minMz: Double,
    maxMz: Double,
    minTime: Float,
    maxTime: Float): Array[Peakel] = {

    // Retrieve peakel ids using the in-memory R*Tree
    val peakelIds = if (rTreeOpt.isDefined) {
      findPeakelIdsInRange(rTreeOpt.get, minMz, maxMz, minTime, maxTime)
    } // Retrieve peakel ids using the peakelDB R*Tree
    else {
      PeakelDbReader.findPeakelIdsInRangeFromPeakelDB(sqliteConn, minMz, maxMz, minTime, maxTime)
    }

    //assert(peakelIdsMem.length == peakelIds.length, "something wrong in Rtrees")

    val peakels = PeakelDbReader._loadPeakelsForIds(sqliteConn, peakelIds).toArray

    peakels
  }
  
    
   def findCorrelatingPeakel(ref: Peakel, peakels: Array[Peakel]): Option[Peakel] = {
    
    val correlations = peakels.map { peakel => 
      computeCorrelation(ref,peakel) -> peakel
    }
    val (correlation, bestPeakel) = correlations.maxBy(_._1)

    if (correlation > MIN_CORRELATION_THRESHOLD) Some(bestPeakel)
    else None
  }

  def findMatchingPeakel(
      coelutingPeakels: Seq[Peakel],
      putativeFt: PutativeFeature,
      mappingMozTolPpm: Float,
      extractionMozTolPpm: Float,
      assignedMzDbPeakelIdSetOpt: Option[HashSet[Int]],
      multiMatchedMzDbPeakelIdsOpt: Option[Seq[Int]],
      metric: Metric
  ): Option[(Peakel,Boolean)] = {

    val peakelMz = putativeFt.mz
    val charge = putativeFt.charge
    val minTime = putativeFt.elutionTime - putativeFt.elutionTimeTolerance
    val avgTime = putativeFt.elutionTime
    val maxTime = putativeFt.elutionTime + putativeFt.elutionTimeTolerance

    val mozTolInDa = MsUtils.ppmToDa(peakelMz, mappingMozTolPpm)

    val foundPeakels = coelutingPeakels.filter { peakel =>
      (math.abs(peakelMz - peakel.getApexMz) <= mozTolInDa) && (peakel.getElutionTime() >= minTime) && (peakel.getElutionTime() <= maxTime)
    }

    if (foundPeakels.isEmpty) {
      metric.incr("missing peakel: no peakel found in the peakelDB")
      return None
    }

    // Apply some filters to the found peakels: they must not be already assigned or included in a pool of peakels
    // that could be matched multiple times
    val matchingPeakels = if (multiMatchedMzDbPeakelIdsOpt.isDefined || assignedMzDbPeakelIdSetOpt.isDefined) {
      foundPeakels.filter { foundPeakel => (multiMatchedMzDbPeakelIdsOpt.isDefined && multiMatchedMzDbPeakelIdsOpt.get.contains(foundPeakel.id)) ||
        (!assignedMzDbPeakelIdSetOpt.isDefined || !assignedMzDbPeakelIdSetOpt.get.contains(foundPeakel.id)) }
    } else {
      foundPeakels
    }

    if ((foundPeakels.size - matchingPeakels.size) > 0) {
      metric.storeValue("missing peakel:more found than unassigned", (foundPeakels.size - matchingPeakels.size))
    }

    val filteredPeakels = PeakelsPatternPredictor.assessReliability(
      extractionMozTolPpm,
      coelutingPeakels,
      matchingPeakels,
      charge,
      mozTolInDa
    )

    if (filteredPeakels.isEmpty) {
      metric.incr("missing peakel: no peakel matching")
      None
    } else if(filteredPeakels.length == 1) {
      metric.incr("missing peakel: only one peakel matching")
      if (filteredPeakels.head._2) { metric.incr("missing peakel: only one peakel matching which is reliable") }
      Some(filteredPeakels.head)
    } else {

      var reliablePeakels = filteredPeakels.filter(_._2)
      if (reliablePeakels.isEmpty) {
        metric.incr("no.reliable.peakel.found")
        reliablePeakels = filteredPeakels
      }
      // sort reliablePeakels by mz distance to ensure minBy always returns the same value
      reliablePeakels.sortWith{ (p1,p2) =>  math.abs(peakelMz-p1._1.getApexMz) < math.abs(peakelMz-p2._1.getApexMz) }

      val nearestPeakelInTime = reliablePeakels.minBy { case (peakel,isReliable) =>
        math.abs(avgTime - peakel.calcWeightedAverageTime())
      }

      if (true) { // fake condition to isolate metrics computation
        val nearestFilteredPeakelInTime = filteredPeakels.minBy { case (peakel,isReliable) =>
          math.abs(avgTime - peakel.calcWeightedAverageTime())
        }
        if (nearestFilteredPeakelInTime != nearestPeakelInTime) { metric.incr("nearestPeakelInTime.not.reliable") }
        metric.addValue("missing peakel: delta moz", MsUtils.DaToPPM( peakelMz, peakelMz-nearestPeakelInTime._1.getApexMz()))
      }

      Some(nearestPeakelInTime)
    }
  }

  def findFeatureIsotopes(
      sqliteConn: SQLiteConnection,
      rTreeOpt: Option[RTree[java.lang.Integer,geometry.Point]],
      peakel: Peakel,
      charge: Int,
      mozTolPpm: Double
  ): Array[Peakel] = {

    val peakelMz = peakel.getMz
    val mozTolInDa = MsUtils.ppmToDa(peakelMz, mozTolPpm)

    val peakelRt = peakel.getElutionTime
    val peakelDurationTol = 1 + math.min(peakel.getElutionTime - peakel.getFirstElutionTime, peakel.getLastElutionTime - peakel.getElutionTime)
    val minRt = peakelRt - peakelDurationTol
    val maxRt = peakelRt + peakelDurationTol

    val pattern = IsotopePatternEstimator.getTheoreticalPattern(peakelMz, charge)
    val intensityScalingFactor = peakel.getApexIntensity / pattern.mzAbundancePairs(0)._2

    val isotopes = new ArrayBuffer[Peakel](pattern.isotopeCount)
    isotopes += peakel

    breakable {
      // Note: skip first isotope because it is already included in the isotopes array
      for (isotopeIdx <- 1 until pattern.isotopeCount) {

        val prevIsotope = isotopes.last
        val ipMoz = prevIsotope.getMz + (MolecularConstants.AVERAGE_PEPTIDE_ISOTOPE_MASS_DIFF / charge)

        // Search for peakels corresponding to second isotope
        val foundPeakels = PeakelDbHelper.findPeakelsInRange(
          sqliteConn,
          rTreeOpt,
          ipMoz - mozTolInDa,
          ipMoz + mozTolInDa,
          minRt,
          maxRt
        )

        if (foundPeakels.nonEmpty) {

          val isotopePeakel = PeakelDbHelper.findCorrelatingPeakel(peakel, foundPeakels)

          val expectedIntensity = pattern.mzAbundancePairs(isotopeIdx)._2 * intensityScalingFactor
          if (isotopePeakel.isDefined) {
            // Gentle constraint on the observed intensity: no more than 4 times the expected intensity
            if (isotopePeakel.get.getApexIntensity() < 4 * expectedIntensity) {
              isotopes += isotopePeakel.get
            } else {
              break
            }
          } else {
            break
          }
        } else {
          break
        }
      }
    }

    isotopes.toArray
  }

  def computeCorrelation(p1: Peakel, p2: Peakel): Double = {

    val (y1, y2) = zipPeakelIntensities(p1, p2)
    PEARSON.correlation(y1, y2)

  }
 
  private def zipPeakelIntensities(p1: Peakel, p2: Peakel): (Array[Double], Array[Double]) = {

    val p1Intensities = p1.getIntensityValues()
    val p2Intensities = p2.getIntensityValues()
    var indexes = {p1.spectrumIds ++ p2.spectrumIds}.toList.distinct.sortWith((a,b) => a < b)
    
    def _buildIntensityVector(p: Peakel): Array[Double] = {
      val y = Array.fill(indexes.size){0.0} 

      var index = 0;
      var peakelCursor = p.getNewCursor()
      while (peakelCursor.next()) {
        while ((index < indexes.length) && (indexes(index) < peakelCursor.getSpectrumId())) {
          index += 1
        }
        if (indexes(index) == peakelCursor.getSpectrumId()) {
          y(index) = peakelCursor.getIntensity()
        }
      }
      y
    }
    
    (_buildIntensityVector(p1),_buildIntensityVector(p2))
  }

}