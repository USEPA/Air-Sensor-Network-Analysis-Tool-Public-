/******************************************************************************
PURPOSE: ASNAT_Utilities.cpp - C++ utility routines used by ASNAT_Utilities.R
         to boost performance by 100x.
HISTORY: 2023-01-10 plessel.todd@epa.gov
STATUS:  unreviewed tested
******************************************************************************/


//================================ INCLUDES ===================================

#include <math.h>   // For sqrt(), cos(), floor().
#include <string.h> // For strncmp().
#include <stdlib.h> // For atoi().
#include <limits.h> // For INT_MAX.

#include <list> // For std::list.

#include <Rcpp.h> // For Rcpp::NumericVector, etc.

//============================ PRIVATE FUNCTIONS ===============================


/******************************************************************************
PURPOSE: ASNAT_nearest_point_cpp - Get distance (in meters) and index of point
         nearest a given point.
INPUTS:  const double longitude          Longitude of reference point.
         const double latitude           Latitude of reference point.
         const int count                 Number of points in longitudes[].
         const double* const longitudes  Longitudes of points to check.
         const double* const latitudes   Latitudes  of points to check.
OUTPUTS: int* const nearest_index        Index into longitudes, latitudes of
                                         point nearest the reference point.
RETURNS: double distance in meters between reference point and nearest point.
******************************************************************************/

static double ASNAT_nearest_point_cpp(const double longitude,
                                      const double latitude,
                                      const int count,
                                      const double* const longitudes,
                                      const double* const latitudes,
                                      int* const nearest_index) {

  double nearest_distance_degrees = 1e30;
  int the_nearest_index = 0;
  *nearest_index = 0;

  for (int index = 0; index < count; ++index ) {
    const double this_longitude = longitudes[index];
    double longitude_distance = longitude - this_longitude;

    if (longitude_distance < 0.0) {
      longitude_distance = -longitude_distance;
    }

    if (longitude_distance < nearest_distance_degrees) {
      const double this_latitude = latitudes[index];
      double latitude_distance = latitude - this_latitude;

      if (latitude_distance < 0.0) {
        latitude_distance = -latitude_distance;
      }

      if (latitude_distance < nearest_distance_degrees) {
        const double distance_degrees =
          sqrt(longitude_distance * longitude_distance +
          latitude_distance * latitude_distance);

        if (distance_degrees < nearest_distance_degrees) {
          nearest_distance_degrees = distance_degrees;
          the_nearest_index = index;
        }
      }
    }
  }

  *nearest_index = the_nearest_index;
  const double nearest_longitude = longitudes[the_nearest_index];
  const double nearest_latitude = latitudes[the_nearest_index];

  // Compute distance in meters. http://en.wikipedia.org/wiki/Lat-lon

  static const double to_radians = 0.017453292519943;
  static const double meters_per_degree_equator = 111132.954;
  const double mean_latitude_radians =
    (latitude + nearest_latitude) * 0.5 * to_radians;
  const double mean_latitude_radians_2 =
    mean_latitude_radians + mean_latitude_radians;
  const double mean_latitude_radians_4 =
    mean_latitude_radians_2 + mean_latitude_radians_2;

  const double meters_per_degree_longitude =
    meters_per_degree_equator * cos(mean_latitude_radians);
  const double meters_per_degree_latitude =
    meters_per_degree_equator - 559.822 * cos(mean_latitude_radians_2) +
    1.175 * cos(mean_latitude_radians_4);

  const double delta_longitude = longitude - nearest_longitude;
  const double delta_latitude = latitude - nearest_latitude;

  const double delta_longitude_meters =
    delta_longitude * meters_per_degree_longitude;
  const double delta_latitude_meters =
    delta_latitude * meters_per_degree_latitude;
  const double delta_longitude_meters_squared =
    delta_longitude_meters * delta_longitude_meters;
  const double delta_latitude_meters_squared =
    delta_latitude_meters * delta_latitude_meters;

  const double nearest_distance_meters =
    sqrt(delta_longitude_meters_squared + delta_latitude_meters_squared);

  const double result = nearest_distance_meters;
  return result;
}



/******************************************************************************
PURPOSE: ASNAT_is_nearby_point_cpp - Is point (longitude1, latitude1) within
         delta_meters of point (longitude2, latitude2)?
INPUTS:  const double delta_meters  Meters distance to check.
         const double longitude1    Longitude of 1st point.
         const double latitude1     Latitude  of 1st point.
         const double longitude2    Longitude of 2nd point.
         const double latitude2     Latitude  of 2nd point.
RETURNS: bool true if within distance, else false.
******************************************************************************/

static bool ASNAT_is_nearby_point_cpp(const double delta_meters,
                                      const double longitude1,
                                      const double latitude1,
                                      const double longitude2,
                                      const double latitude2) {

  static const double to_radians = 0.017453292519943;
  static const double meters_per_degree_equator = 111132.954;
  bool result = false;

  // Compute distance in meters.
  // http://en.wikipedia.org/wiki/Lat-lon

  const double mean_latitude_radians =
    (latitude1 + latitude2) * 0.5 * to_radians;

  const double  meters_per_degree_longitude =
    meters_per_degree_equator * cos(mean_latitude_radians);
  double delta_longitude = longitude1 - longitude2;

  if (delta_longitude < 0.0) {
    delta_longitude = -delta_longitude;
  }

  const double delta_longitude_meters =
    delta_longitude * meters_per_degree_longitude;

  if (delta_longitude_meters <= delta_meters) {
    const double mean_latitude_radians_2 =
      mean_latitude_radians + mean_latitude_radians;
    const double mean_latitude_radians_4 =
      mean_latitude_radians_2 + mean_latitude_radians_2;
    const double meters_per_degree_latitude =
      meters_per_degree_equator -
      559.822 * cos(mean_latitude_radians_2) +
      1.175 * cos(mean_latitude_radians_4);
    double delta_latitude = latitude1 - latitude2;

    if (delta_latitude < 0.0) {
      delta_latitude = -delta_latitude;
    }

    const double delta_latitude_meters =
      delta_latitude * meters_per_degree_latitude;

    if (delta_latitude_meters <= delta_meters) {
      const double delta_longitude_meters_squared =
        delta_longitude_meters * delta_longitude_meters;
      const double delta_latitude_meters_squared =
        delta_latitude_meters * delta_latitude_meters;
      const double distance_meters =
        sqrt(delta_longitude_meters_squared + delta_latitude_meters_squared);
      result = distance_meters <= delta_meters;
    }
  }

  return result;
}



/******************************************************************************
PURPOSE: ASNAT_compare_datasets_cpp0 - Get lists of indices of
         time-matched points within a given distance to each other.
INPUTS:  const double delta_meters              Minimum distance to match.
         const int timestamp_length             13 if hourly, 10 if daily.
         const int count_x                      Number of X points.
         const Rcpp::StringVector& timestamps_x String timestamps of X.
         const double* const longitudes_x       Longitudes of points in X.
         const double* const latitudes_x        Latitudes of points in X.
         const int count_y                      Number of Y points.
         const Rcpp::StringVector& timestamps_y String timestamps of Y.
         const double* const longitudes_y       Longitudes of points in Y.
         const double* const latitudes_y        Latitudes of points in Y.
OUTPUTS: std::list<int> result_x                List of indices into X of pairs
         std::list<int> result_y                List of indices into Y of pairs
******************************************************************************/

static void ASNAT_compare_datasets_cpp0(const double delta_meters,
                                        const int timestamp_length,
                                        const int count_x,
                                        const Rcpp::StringVector& timestamps_x,
                                        const double* const longitudes_x,
                                        const double* const latitudes_x,
                                        const int count_y,
                                        const Rcpp::StringVector& timestamps_y,
                                        const double* const longitudes_y,
                                        const double* const latitudes_y,
                                        std::list<int>& result_x,
                                        std::list<int>& result_y) {

  int start_index_y = 0;
  int result_count = 0;
  result_x.clear();
  result_y.clear();

  for (int index_x = 0; index_x < count_x; ++index_x) {
    const double longitude_x = longitudes_x[index_x];
    const double latitude_x = latitudes_x[index_x];
    const Rcpp::String& timestamp_x(timestamps_x[index_x]);
    const char* const c_timestamp_x = timestamp_x.get_cstring();
    int timestamp_matches = 0;

    for (int index_y = start_index_y; index_y < count_y; ++index_y) {
      const Rcpp::String& timestamp_y(timestamps_y[index_y]);
      const char* const c_timestamp_y = timestamp_y.get_cstring();
      const int timestamp_comparison =
        strncmp(c_timestamp_x, c_timestamp_y, timestamp_length);

      // Note: timestamps are (assumed to be) sorted
      // so the break logic below can be used to shorten this inner loop.

      if (timestamp_comparison < 0) {
        start_index_y = index_y - timestamp_matches;
        break;
      }

      if (timestamp_comparison == 0) {
        const double longitude_y = longitudes_y[index_y];
        const double latitude_y = latitudes_y[index_y];
        const bool is_neighbor =
          ASNAT_is_nearby_point_cpp(delta_meters,
                                    longitude_x, latitude_x,
                                    longitude_y, latitude_y);

        ++timestamp_matches;

        if (is_neighbor && result_count < INT_MAX) {
          result_x.push_back(index_x);
          result_y.push_back(index_y);
          ++result_count;
        }
      }
    }
  }
}



//============================ PUBLIC FUNCTIONS ===============================



/******************************************************************************
PURPOSE: ASNAT_nearest_site_cpp - Get nearest site id and distance to
         point (longitude, latitude).
INPUTS:  const double longitude                  Longitude of reference point.
         const double latitude                   Latitude  of reference point.
         const Rcpp::NumericVector& longitudes   Longitudes  of points.
         const Rcpp::NumericVector& latitudes    Latitudes   of points.
         const Rcpp::IntegerVector& site_ids     site ids of points.
RETURNS: Rcpp::List containing 2 named items:
         "id" = int site_id and "distance" = double distance in meters to
         the point (longitude, latitude).
NOTES: The export comment below is required!
******************************************************************************/

// [[Rcpp::export]]
Rcpp::List ASNAT_nearest_site_cpp(const double longitude,
                                     const double latitude,
                                     const Rcpp::NumericVector& longitudes,
                                     const Rcpp::NumericVector& latitudes,
                                     const Rcpp::IntegerVector& site_ids) {

  const int count = longitudes.length();
  const double* const longitudes0 = longitudes.begin();
  const double* const latitudes0 = latitudes.begin();
  int nearest_index = 0;
  const double nearest_other_site_distance =
    ASNAT_nearest_point_cpp(longitude, latitude, count,
                            longitudes0, latitudes0,
                            &nearest_index);
  const int nearest_other_site_id = site_ids[nearest_index];
  return Rcpp::List::create(Rcpp::Named("id") = nearest_other_site_id,
                            Rcpp::Named("distance") =
                              nearest_other_site_distance);
}



/******************************************************************************
PURPOSE: ASNAT_compare_datasets_cpp - Return a pair of arrays of indices into
         two arrays of matched points i.e., points that are at the same time
         and within delta_meters apart.
INPUTS:  const Rcpp::DataFrame& data_frame_x  1st data frame to compare.
         const Rcpp::DataFrame& data_frame_y  2nd data frame to compare.
         const double delta_meters            Minimum distance to match.
         const bool is_hourly                 1 if hourly time matching.
RETURNS: Rcpp::List  List of two named items "x" and "y" each of which are
         of type Rcpp::IntegerVector and are 1-based indices into X and Y of
         pairs of points within delta_meters.
NOTES: The export comment below is required!
******************************************************************************/

// [[Rcpp::export]]
Rcpp::List ASNAT_compare_datasets_cpp(const Rcpp::DataFrame& data_frame_x,
                                      const Rcpp::DataFrame& data_frame_y,
                                      const double delta_meters,
                                      const bool is_hourly) {

  // Get pointers to internal storage to avoid object function-call overhead.
  // Rcpp::StringVector::const_iterator is not a simple pointer to
  // Rcpp::String
  // and trying iterator -= offset results in a crash so just pass address of
  // Rcpp::StringVector and index it with operator[].

  const int timestamp_length = is_hourly ? 13 : 10;

  const Rcpp::StringVector& timestamps_x(data_frame_x[0]);
  const Rcpp::NumericVector& longitudes_x(data_frame_x[1]);
  const Rcpp::NumericVector& latitudes_x(data_frame_x[2]);
  const double* const longitudes_x_0 = longitudes_x.begin();
  const double* const latitudes_x_0 = latitudes_x.begin();
  const int count_x = timestamps_x.length();

  const Rcpp::StringVector& timestamps_y(data_frame_y[0]);
  const Rcpp::NumericVector& longitudes_y(data_frame_y[1]);
  const Rcpp::NumericVector& latitudes_y(data_frame_y[2]);
  const double* const longitudes_y_0 = longitudes_y.begin();
  const double* const latitudes_y_0 = latitudes_y.begin();
  const int count_y = timestamps_y.length();

  // Use a linked-list to avoid reallocation and copying of an unknown number
  // of matched points:

  std::list<int> list_x;
  std::list<int> list_y;

  ASNAT_compare_datasets_cpp0(delta_meters, timestamp_length,
                              count_x,
                              timestamps_x, longitudes_x_0, latitudes_x_0,
                              count_y,
                              timestamps_y, longitudes_y_0, latitudes_y_0,
                              list_x, list_y);

  // Convert linked-lists of 0-based indices into R vectors of 1-based indices:

  const int count = list_x.size();
  Rcpp::IntegerVector result_x(count);
  Rcpp::IntegerVector result_y(count);
  std::list<int>::const_iterator iterator_x = list_x.begin();
  std::list<int>::const_iterator iterator_y = list_y.begin();

  for (int index = 0; index < count; ++index) {
    int value_x = *iterator_x++;
    int value_y = *iterator_y++;
    ++value_x;
    ++value_y;
    result_x[index] = value_x;
    result_y[index] = value_y;
  }

  return Rcpp::List::create(Rcpp::Named("x") = result_x ,
                            Rcpp::Named("y") = result_y);
}




