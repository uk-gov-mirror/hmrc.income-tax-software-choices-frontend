/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.incometaxsoftwarechoicesfrontend.models

import play.api.libs.json.{Json, Reads, __}

case class SoftwareVendorModel(
  name: String,
  email: Option[String],
  phone: Option[String],
  website: String,
  filters: Map[VendorFilter,Boolean],
  accessibilityStatementLink: Option[String] = None,
  nextRelease: Option[String] = None
) {
  def orderedFilterSubset(subsetFilters: Set[VendorFilter]): Map[VendorFilter,Boolean] = {
    val filtersFromVendor = filters.filter(filter => subsetFilters.contains(filter._1)).toSet
    val alwaysDisplayedFilters = subsetFilters.filter(_.alwaysDisplay).map(_ -> true) // What is this used for
    (filtersFromVendor ++ alwaysDisplayedFilters).toSeq.sortBy(_._1.priority).toMap
  }
}

object SoftwareVendorModel {
  implicit val reads: Reads[SoftwareVendorModel] = Json.reads[SoftwareVendorModel]
}

//sealed trait FeatureSupport {
//  val key: String
//}
//
//object FeatureSupport {
//  case object CurrentFeature extends FeatureSupport{
//    override val key: String = "current"
//  }
//
//  case object FutureFeature extends FeatureSupport{
//    override val key: String = "future"
//  }
//
//  implicit val reads: Reads[FeatureSupport] = __.read[String] match {
//    case "current" => CurrentFeature
//    case "future" => FutureFeature
//  }
//
//}