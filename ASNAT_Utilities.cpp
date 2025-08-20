/******************************************************************************
PURPOSE: ASNAT_Utilities.cpp - C++ utility routines used by ASNAT_Utilities.R
         to boost performance by 100x.
HISTORY: 2023-01-10 plessel.todd@epa.gov
STATUS:  unreviewed tested
******************************************************************************/


//================================ INCLUDES ===================================

#include <math.h>   // For sqrt(), cos(), floor().
#include <float.h>  // For DBL_MAX.
#include <string.h> // For strncmp().
#include <stdlib.h> // For atoi().
#include <limits.h> // For INT_MAX.
#include <stdio.h>  // For snprintf().

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



/******************************************************************************
PURPOSE: ASNAT_signed_area_of_polygon_cpp - Signed area of a single contour of
         a polygon.
INPUTS:  const size_t count  Number of vertices in polygon.
         const double x[]    X-coordinates of vertices.
         const double y[]    Y-coordinates of vertices.
RETURNS: double signed area of polygon.
         Negative if vertices are in clockwise order.
NOTES:   http://mathworld.wolfram.com/PolygonArea.html
******************************************************************************/

static double ASNAT_signed_area_of_polygon_cpp( const size_t count,
                                                const double x[],
                                                const double y[] ) {
  double result = 0.0;
  size_t index = 0;

  for ( index = 0; index < count; ++index ) {
    const size_t indexp1 = index + 1;
    const size_t index1 = indexp1 < count ? indexp1 : 0;
    const double triangleArea =
      x[ index ] * y[ index1 ] - x[ index1 ] * y[ index ];
    result += triangleArea;
  }

  result *= 0.5;
  return result;
}



//============================ PUBLIC FUNCTIONS ===============================



/******************************************************************************
PURPOSE: ASNAT_clip_polygon_cpp - Clip polygon to an axis-aligned rectangle
         and return the number of vertices in clipped polygon.
INPUTS:  const double clipXMin  X-coordinate of lower-left  corner of clip rect
         const double clipYMin  Y-coordinate of lower-left  corner of clip rect
         const double clipXMax  X-coordinate of upper-right corner of clip rect
         const double clipYMax  Y-coordinate of upper-right corner of clip rect
         const Rcpp::NumericVector& xr X-coordinates of input polygon to clip.
         const Rcpp::NumericVector& yr X-coordinates of input polygon to clip.
         Rcpp::NumericVector& cx[2 * count + 2] Clipped X-coordinates storage.
         Rcpp::NumericVector& cy[2 * count + 2] Clipped Y-coordinates storage.
OUTPUTS: Rcpp::NumericVector& cx[result]      Clipped X-coordinates.
         Rcpp::NumericVector& cy[result]      Clipped Y-coordinates.
RETURNS: int number of vertices in clipped polygon.
NOTES:   Uses the Liang-Barsky polygon clipping algorithm. (Fastest known.)
         "An Analysis and Algorithm for Polygon Clipping",
         You-Dong Liang and Brian Barsky, UC Berkeley,
         CACM Vol 26 No. 11, November 1983.
         https://www.longsteve.com/fixmybugs/?page_id=210
         The export comment below is required!
******************************************************************************/

// [[Rcpp::export]]
int ASNAT_clip_polygon_cpp( const double clipXMin,
                            const double clipYMin,
                            const double clipXMax,
                            const double clipYMax,
                            const Rcpp::NumericVector& xr,
                            const Rcpp::NumericVector& yr,
                            Rcpp::NumericVector& cxr,
                            Rcpp::NumericVector& cyr ) {
  int result = 0;
  const int count = xr.length();
  const double* const x = xr.begin();
  const double* const y = yr.begin();
  double* const cx = cxr.begin();
  double* const cy = cyr.begin();
  const double inf = DBL_MAX;
  double xIn   = 0.0; /* X-coordinate of entry point. */
  double yIn   = 0.0; /* Y-coordinate of entry point. */
  double xOut  = 0.0; /* X-coordinate of exit point. */
  double yOut  = 0.0; /* Y-coordinate of exit point. */
  double tInX  = 0.0; /* Parameterized X-coordinate of entry intersection. */
  double tInY  = 0.0; /* Parameterized Y-coordinate of entry intersection. */
  double tOutX = 0.0; /* Parameterized X-coordinate of exit intersection. */
  double tOutY = 0.0; /* Parameterized Y-coordinate of exit intersection. */
  int vertex = 0;

  for ( vertex = 0; vertex < count; ++vertex ) {
    const int vertexp1 = vertex + 1;
    const int vertex1 = vertexp1 < count ? vertexp1 : 0;
    const double vx = x[ vertex ];
    const double vy = y[ vertex ];
    const double deltaX = x[ vertex1 ] - vx; /* Edge direction. */
    const double deltaY = y[ vertex1 ] - vy;
    const double oneOverDeltaX = deltaX ? 1.0 / deltaX : 0.0;
    const double oneOverDeltaY = deltaY ? 1.0 / deltaY : 0.0;
    double tOut1 = 0.0;
    double tOut2 = 0.0;
    double tIn2 = 0.0;

    /*
     * Determine which bounding lines for the clip window the containing line
     * hits first:
     */

    if ( deltaX > 0.0 || ( deltaX == 0.0 && vx > clipXMax ) ) {
      xIn  = clipXMin;
      xOut = clipXMax;
    } else {
      xIn  = clipXMax;
      xOut = clipXMin;
    }

    if ( deltaY > 0.0 || ( deltaY == 0.0 && vy > clipYMax ) ) {
      yIn  = clipYMin;
      yOut = clipYMax;
    } else {
      yIn  = clipYMax;
      yOut = clipYMin;
    }

    /* Find the t values for the x and y exit points: */

    if ( deltaX != 0.0 ) {
      tOutX = ( xOut - vx ) * oneOverDeltaX;
    } else if ( vx <= clipXMax && clipXMin <= vx ) {
      tOutX = inf;
    } else {
      tOutX = -inf;
    }

    if ( deltaY != 0.0 ) {
      tOutY = ( yOut - vy ) * oneOverDeltaY;
    } else if ( vy <= clipYMax && clipYMin <= vy ) {
      tOutY = inf;
    } else {
      tOutY = -inf;
    }

    /* Set tOut1 = min( tOutX, tOutY ) and tOut2 = max( tOutX, tOutY ): */

    if ( tOutX < tOutY ) {
      tOut1 = tOutX;
      tOut2 = tOutY;
    } else {
      tOut1 = tOutY;
      tOut2 = tOutX;
    }

    if ( tOut2 > 0.0 ) {

      if ( deltaX != 0.0 ) {
        tInX = ( xIn - vx ) * oneOverDeltaX;
      } else {
        tInX = -inf;
      }

      if ( deltaY != 0.0 ) {
        tInY = ( yIn - vy ) * oneOverDeltaY;
      } else {
        tInY = -inf;
      }

      /* Set tIn2 = max( tInX, tInY ): */

      if ( tInX < tInY ) {
        tIn2 = tInY;
      } else {
        tIn2 = tInX;
      }

      if ( tOut1 < tIn2 ) { /* No visible segment. */

        if ( 0.0 < tOut1 && tOut1 <= 1.0 ) {

          /* Line crosses over intermediate corner region. */

          if ( tInX < tInY ) {
            cx[ result ] = xOut;
            cy[ result ] = yIn;
          } else {
            cx[ result ] = xIn;
            cy[ result ] = yOut;
          }

          ++result;
        }
      } else { /* Line crosses through window: */

        if ( 0.0 < tOut1 && tIn2 <= 1.0 ) {

          if ( 0.0 <= tIn2 ) { /* Visible segment: */

            if ( tInX > tInY ) {
              cx[ result ] = xIn;
              cy[ result ] = vy + ( tInX * deltaY );
            } else {
              cx[ result ] = vx + ( tInY * deltaX );
              cy[ result ] = yIn;
            }

            ++result;
          }

          if ( 1.0 >= tOut1 ) {

            if ( tOutX < tOutY ) {
              cx[ result ] = xOut;
              cy[ result ] = vy + ( tOutX * deltaY );
            } else {
              cx[ result ] = vx + ( tOutY * deltaX );
              cy[ result ] = yOut;
            }

            ++result;
          } else {
            cx[ result ] = x[ vertex1 ];
            cy[ result ] = y[ vertex1 ];
            ++result;
          }
        }
      }

      if ( 0.0 < tOut2 && tOut2 <= 1.0 ) {
        cx[ result ] = xOut;
        cy[ result ] = yOut;
        ++result;
      }
    }
  }

  if ( result < 3 ) {
    result = 0; /* Discard any result less than a triangle. */
  } else { /* Check that the clipped polygon(s) are not entirely degenerate: */
    const double clipped_polygon_area =
      ASNAT_signed_area_of_polygon_cpp( result, cx, cy );

    if ( clipped_polygon_area == 0.0 ) {
      result = 0;
    }
  }

  return result;
}



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



/******************************************************************************
PURPOSE: ASNAT_copy_site_measures_cpp - Copy a given site's measures for input
         to Nowcast function to speed-up the Nowcast calculation of a dataset.
INPUTS:  const int site                     Site number.
         const int site_column              Column number (1-based) of site.
         const int measure_column           Column number (1-based) of measure.
         const Rcpp::StringVector& all_timestamps Sorted all yyyy-mm-ddThh.
         const Rcpp::DataFrame& data_frame Data frame containing site, measure.
         Rcpp::NumericVector& measures_with_na  All hourly NA.
OUTPUTS: Rcpp::NumericVector& measures_with_na  All hourly site measures or NA.
NOTES: The export comment below is required!
******************************************************************************/

// [[Rcpp::export]]
void ASNAT_copy_site_measures_cpp(const int site,
                                  const int site_column,
                                  const int measure_column,
                                  const Rcpp::StringVector& all_timestamps,
                                  const Rcpp::DataFrame& data_frame,
                                  Rcpp::NumericVector& measures_with_na) {

  // Get pointers to internal storage to avoid object function-call overhead.
  // Rcpp::StringVector::const_iterator is not a simple pointer to
  // Rcpp::String
  // and trying iterator -= offset results in a crash so just pass address of
  // Rcpp::StringVector and index it with operator[].

  const Rcpp::StringVector& site_timestamps(data_frame[0]);
  const Rcpp::IntegerVector& sites(data_frame[site_column - 1]);
  const Rcpp::NumericVector& measures(data_frame[measure_column - 1]);
  const int* const sites_0 = sites.begin();
  const double* const measures_0 = measures.begin();
  double* const measures_with_na_0 = measures_with_na.begin();
  const int timesteps = all_timestamps.length();
  const int rows = sites.length();
  const int timestamp_length = 13; // E.g., 2022-06-01T17.

  for (int timestep = 0; timestep < timesteps; ++timestep) {
    const Rcpp::String& timestamp(all_timestamps[timestep]);
    const char* const c_timestamp = timestamp.get_cstring();

    for (int row = 0; row < rows; ++row) {
      const int row_site = sites_0[row];

      if (row_site == site) {
        const Rcpp::String& site_timestamp(site_timestamps[row]);
        const char* const c_site_timestamp = site_timestamp.get_cstring();
        const int timestamp_comparison =
          strncmp(c_site_timestamp, c_timestamp, timestamp_length);

        if (timestamp_comparison == 0) {
          const double measure = measures_0[row];
          measures_with_na_0[timestep] = measure;
          row = rows - 1; // At most one site row per hour so stop looping.
        } else if (timestamp_comparison > 0) {
          row = rows - 1; // Timestamps are sorted so stop looping if past.
        }
      }
    }
  }
}



/******************************************************************************
PURPOSE: ASNAT_copy_nowcast_measures_cpp - Copy a given site's nowcast output
         from Nowcast function to speed-up the Nowcast calculation of a dataset
INPUTS:  const int site                     Site number.
         const int site_column              Column number (1-based) of site.
         const int nowcast_column           Column number (1-based) of nowcast.
         const Rcpp::StringVector& all_timestamps Sorted all yyyy-mm-ddThh.
         const Rcpp::NumericVector& nowcast_with_na  All hourly site nowcast
                                                     values or NA.
         Rcpp::DataFrame& data_frame Data frame containing site, nowcast (=NA).
OUTPUTS: Rcpp::DataFrame& data_frame Data frame containing site, nowcast.
NOTES: The export comment below is required!
******************************************************************************/

// [[Rcpp::export]]
void ASNAT_copy_nowcast_measures_cpp(const int site,
                                     const int site_column,
                                     const int nowcast_column,
                                     const Rcpp::StringVector& all_timestamps,
                                     const Rcpp::NumericVector& nowcast_with_na,
                                     Rcpp::DataFrame& data_frame) {

  // Get pointers to internal storage to avoid object function-call overhead.
  // Rcpp::StringVector::const_iterator is not a simple pointer to
  // Rcpp::String
  // and trying iterator -= offset results in a crash so just pass address of
  // Rcpp::StringVector and index it with operator[].

  const Rcpp::StringVector& site_timestamps(data_frame[0]);
  const Rcpp::IntegerVector& sites(data_frame[site_column - 1]);
//Rcpp::NumericVector& nowcasts(data_frame[nowcast_column - 1]); // Why & wrong?
  Rcpp::NumericVector nowcasts(data_frame[nowcast_column - 1]); // Why no &?
  const int* const sites_0 = sites.begin();
  double* const site_nowcasts_0 = nowcasts.begin();
  const double* const nowcast_with_na_0 = nowcast_with_na.begin();
  const int timesteps = all_timestamps.length();
  const int rows = sites.length();
  const int timestamp_length = 13; // E.g., 2022-06-01T17.

  for (int timestep = 0; timestep < timesteps; ++timestep) {
    const double nowcast = nowcast_with_na_0[timestep];

    // Since the data_frame nowcast column vector is already initialized to NA
    // there is no need to search and overwrite it with NA.

    if (!R_IsNA(nowcast)) {
      const Rcpp::String& timestamp(all_timestamps[timestep]);
      const char* const c_timestamp = timestamp.get_cstring();

      for (int row = 0; row < rows; ++row) {
        const int row_site = sites_0[row];

        if (row_site == site) {
          const Rcpp::String& site_timestamp(site_timestamps[row]);
          const char* const c_site_timestamp = site_timestamp.get_cstring();
          const int timestamp_comparison =
            strncmp(c_site_timestamp, c_timestamp, timestamp_length);

          if (timestamp_comparison == 0) {
            site_nowcasts_0[row] = nowcast;
            row = rows - 1; // At most one site row per hour so stop looping.
          } else if (timestamp_comparison > 0) {
            row = rows - 1; // Timestamps are sorted so stop looping if past.
          }
        }
      }
    }
  }
}



/******************************************************************************
PURPOSE: ASNAT_get_sites_in_bounds_cpp - Return a string vector containing
         "site note" for all sites located within bounds.
INPUTS:  const double west           West coordinate of bounds.
         const double east           East coordinate of bounds.
         const double south          South coordinate of bounds.
         const double north          North coordinate of bounds.
         const Rcpp::String& first   First entry of result.
 const Rcpp::DataFrame& data_frame  Data frame containing site info:
                                      column 0 = longitudes
                                      column 1 = latitudes
                                      column 2 = ids
                                      column 3 = notes
OUTPUTS: Rcpp::StringVector          Vector of "site note" for sites in bounds.
NOTES: The export comment below is required!
******************************************************************************/

// [[Rcpp::export]]
Rcpp::StringVector ASNAT_get_sites_in_bounds_cpp(const double west,
                                                 const double east,
                                                 const double south,
                                                 const double north,
                                                 const Rcpp::String& first,
                                                 const Rcpp::DataFrame&
                                                   data_frame) {

  // Get pointers to internal storage to avoid object function-call overhead.
  // Rcpp::StringVector::const_iterator is not a simple pointer to
  // Rcpp::String
  // and trying iterator -= offset results in a crash so just pass address of
  // Rcpp::StringVector and index it with operator[].

  const Rcpp::NumericVector& longitudes(data_frame[0]);
  const Rcpp::NumericVector& latitudes(data_frame[1]);
  const Rcpp::NumericVector& ids(data_frame[2]);
  const Rcpp::StringVector& notes(data_frame[3]);
  const double* const longitudes_0 = longitudes.begin();
  const double* const latitudes_0 = latitudes.begin();
  const double* const ids_0 = ids.begin();
  const int rows = longitudes.length();
  Rcpp::IntegerVector matched_rows(rows, 0);
  int* const matched_rows_0 = matched_rows.begin();
  int match_count = 0;

  for (int row = 0; row < rows; ++row) {
    const double longitude = longitudes_0[row];

    if (longitude >= west && longitude <= east) {
      const double latitude = latitudes_0[row];

      if (latitude >= south && latitude <= north) {
        matched_rows_0[match_count] = row;
        ++match_count;
      }
    }
  }

  Rcpp::StringVector result(match_count + 1, "");
  result[0] = first;

  for (int row = 0; row < match_count; ++row) {
    const int matched_row = matched_rows_0[row];
    const int id = (int) ids_0[matched_row];
    const Rcpp::String& note(notes[matched_row]);
    const char* const c_note = note.get_cstring();
    char buffer[ 80 ] = "";
    snprintf(buffer, sizeof buffer / sizeof *buffer, "%d %s", id, c_note);
    result[row + 1] = buffer;
  }

  return result;
}



