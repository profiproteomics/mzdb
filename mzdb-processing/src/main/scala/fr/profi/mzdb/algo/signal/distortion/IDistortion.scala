package fr.profi.mzdb.algo.signal.distortion

import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.Feature

trait IDistortion {

  def distort

}

trait IPeakelDistortion extends IDistortion {

  def distort( peakel: Peakel ): Peakel

}

trait IFeatureDistortion extends IDistortion {


  def distort( feature: Feature ): Feature
}