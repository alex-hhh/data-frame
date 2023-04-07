#lang scribble/manual
@require[scribble/example
         @for-label[racket/base
                    racket/contract
                    math/statistics
                    plot
                    (only-in plot/utils renderer2d?)
                    db
                    data-frame]]

@(define ev
   (let ([eval (make-base-eval)])
     (eval '(require data-frame))
     eval))

@title{data-frame}
@author{Alex Harsányi}

@defmodule[data-frame]

A data frame is a data structure used to hold data in tables with rows and
columns.  It is meant for conveninent access and manipulation of relatively
large data sets that fit in the process memory.  The package also provides
functions for loading and saving data from data frames from various formats,
as well as several utilities and helper functions for statistical
calculations, plotting and curve fitting.

The package was initially written to support data the data processing needs
for @hyperlink["https://github.com/alex-hhh/ActivityLog2"]{ActivityLog2}, and
since that application mostly deals with time series based data, the package
supports data which is naturally ordered (whether by a timestamp or some other
data column).  However, the package does support more general data which is
not necessarily ordered and also supports efficient lookups using secondary
indexes, allowing multiple traversal orders to be defined for the data.

With data science a popular topic, a package named @racket[data-frame]
inevitably brings the question of how does it compare with other
implementations with similar names and descriptions.  The package author does
not know the answer to that question, except to say that any resemblance to
other implementatins is purely coincidental.

@section{Creating data frames}

The functions below allow constructing new data frames.  They are mainly
intended for writing functions that load data into data frames from different
sources, rather for direct use in other programs.  To load data into data
frames, see @racket[df-read/csv] or @racket[df-read/sql] and for manually
creating data frames, @racket[for/data-frame].

@defproc[(data-frame? (df any/c)) boolean?]{

  Return @racket[#t] if @racket[df] is a data frame.

}

@defproc[(series? (series any/c)) boolean?]{

  Return @racket[#t] if @racket[series] is a data series

}

@defproc[(make-data-frame) data-frame?]{

  Return a new, empty, data frame.  The data frame does not contain any
  series, and its size will be determined by the first series added, see also
  @racket[df-add-series!].

}

@defproc[(make-series (name string?)
                      (#:data data vector?)
                      (#:cmpfn cmpfn (or/c #f (-> any/c any/c boolean?)))
                      (#:na na any/c)
                      (#:contract contractfn (-> any/c boolean?)))
                      series?]{

  Create a new data series named @racket[name] with contents from
  @racket[data].

  @racket[cmpfn] specifies an ordering function to use.  If present, values
  can be looked up in this series using @racket[df-index-of] and
  @racket[df-lookup].  The data must be ordered according to this function

  @racket[na] specifies the "not available" value for this series, by default
  it is @racket[#f].

  @racket[contractfn] is a contract function.  If present, all values in the
  data series, except NA values, must satisfy this contract, that is, the
  function must return @racket[#t] for all values in the series.

}

@defproc[(df-add-series! (df data-frame?) (series series?)) any/c]{

  Add a new series to the data frame.  If the data frame contains no other
  series, this series can have any number of elements, otherwise it must have
  the same number of elements as the other series in the data frame.

  If the data frame already contains a series with the same name, that series
  will be replaced.

  See also @racket[df-row-count] and @racket[make-series].

}

@defproc[(df-del-series! (df data-frame?) (name string?)) any/c]{

  Remove the series named @racket[name] from the data frame @racket[df].  Does
  nothing if the series does not exist.

}

@defproc[(df-rename-series! (df data-frame?) (old-name string?) (new-name string?)) any/c]{

  Rename a series in the data frame @racket[df] from @racket[old-name] to
  @racket[new-name].  A series named @racket[old-name] must exist, otherwise
  an error is signaled.

  This operation will materialize all lazy series (see @racket[df-add-lazy!]),
  making it a possibly costly operation if you have lazy series.  Indices
  using @racket[old-name] will also be updated to use @racket[new-name], but
  this operation is fast, and no re-indexing will be done.

}

@defproc[(df-add-derived! (df data-frame?) (name string?) (base-series (listof string?)) (value-fn mapfn/c)) any/c]{

  Add a new series named @racket[name] to the data frame @racket[df] with
  values that are computed from existing series.  The data for the series is
  created using @racket[df-map] by applying @racket[value-fn] on
  @racket[base-series] and the resulting data is added to the data frame.  See
  @racket[df-map] for notes on the @racket[value-fn].

  If a series named @racket[name] already exists in the data frame, it will be
  replaced.

}

@defproc[(df-add-lazy! (df data-frame?) (name string?) (base-series (listof string?)) (value-fn mapfn/c)) any/c]{

  Add a new series to the data frame, same as @racket[df-add-derived!], but
  delay creating it until it is referenced.  This function allows adding many
  series to a data frame, with the expectation that the cost to create those
  series is paid when (and if) they are used.

  See @racket[df-add-derived!] for the parameter names.

}

@defproc[(df-row-count (df data-frame?)) exact-nonnegative-integer?]{

  Return the number of rows in the data frame @racket[df].  All series inside
  the data frame have the same number of rows.

}

@defproc[(df-series-names (df data-frame?)) (listof string?)]{

  Return a list of the names of all series in the data frame @racket[df].  The
  names are returned in an unspecified order.

}

@deftogether[(@defproc[(df-contains? (df (data-frame?)) (series string?) ...) boolean?]
             @defproc[(df-contains/any? (df (data-frame?)) (series string?) ...) boolean?])]{

  Return @racket[#t] if the data frame @racket[df] the @racket[series]
  specified as arguments.  @racket[df-contains?] returns @racket[#t] only if
  @bold{all} the series are present, while @racket[df-contains/any?] returns
  @racket[#t] if @bold{any} of the specified series are present.

}

@defproc[(df-duplicate-series (df data-frame?) (name string?)) series?]{

  Return a new series by coping the series @racket[name] from the data-frame
  @racket[df]. All data and properties (like sort order, NA value and
  contract) are copied.  New series is intended to be added to another data
  frame using @racket[df-add-series!].

  If the series with name @racket[name] is delayed, see
  @racket[df-add-lazy!], the series will be materialized first.

}

@defproc[(df-set-sorted! (df data-frame?) (name string?) (cmpfn (or/c #f (-> any/c any/c boolean?)))) any/c]{

  Mark an already sorted series @racket[name] in the data frame @racket[df] as
  sorted according to @racket[cmpfn].  Marking a series as sorted, allows it
  to be used for index lookup by @racket[df-index-of] and @racket[df-lookup].
  An error is raised if the series is not actually sorted or if it contains NA
  values.

  If the data in the series is not already sorted, and you want to lookup
  values using @racket[df-index-of] or @racket[df-lookup], consider adding a
  secondary index using @racket[df-add-index!].

}

@defproc[(df-is-sorted? (df data-frame?) (series string?)) boolean?]{

  Return @racket[#t] if the given @racket[series] in the data-frame
  @racket[df] is sorted, that is @racket[df-set-sorted!] has been called and
  succeeded for this series.

}

@defproc[(df-set-contract! (df data-frame?) (name string?) (contractfn (or/c #f (-> any/c boolean?)))) any/c]{

  Set the contract for values in the data frame @racket[df] series
  @racket[name] to @racket[contractfn].  An exception is thrown if not all
  values in the series match @racket[contractfn] or are NA.  The
  @racket[contractfn] need not return @racket[#t] for the NA value.

}

@defproc[(df-shallow-copy [df data-frame?]) data-frame]{

  Creates a copy of @racket[df]. The returned copy will reference the same
  data series objects as the original (and the properties), but any add/delete
  operations, for both series and properties, will only affect the copy,
  however operations on the shared series (like setting a contract) will
  affect both data frames.

}

@deftogether[(@defform[(for/data-frame (series-name ...) (for-clause ...) body-or-break ... body)]
             @defform[(for*/data-frame (series-name ...) (for-clause ...) body-or-break ... body)])]{

  Construct a new data-frame with the given @racket[series-name]s, by
  producing values in the for loop, row by row.  The constructs iterate like
  @racket[for] or @racket[for*], and each @racket[body] must evaluate to a set
  of values that corresponds to each @racket[series-name], in order.

@examples[#:eval ev
  (define df
    (for/data-frame (ints strs)
                    ([int (in-range 5)]
                     [str (in-list (list "a" "b" "c" "d" "e"))])
      (values int str)))
  (df-select df "ints")
  (df-select df "strs")
]

@examples[#:eval ev
  (define df
    (for*/data-frame (ints strs)
                     ([int (in-range 2)]
                      [str (in-list (list "a" "b"))])
      (values int str)))
  (df-select df "ints")
  (df-select df "strs")
]

}

@section{Loading and Saving Data}

The functions construct data frames by loading data from CSV files or by
running an SQL query.

@defproc[(df-read/csv (input (or/c path-string? input-port?))
                      (#:headers? headers? boolean? #t)
                      (#:na na (or/c string? (-> string? boolean?) ""))
                      (#:quoted-numbers? quoted-numbers? boolean? #f))
                      data-frame?]{

  Read CSV data in a data frame from the @racket[input] which is either a port
  or a string, in which case it is assumed to be a file name.  If
  @racket[headers?]  is true, the first row in @racket[input] becomes the
  names of the columns, otherwise, the columns will be named "col1", "col2",
  etc.  The first row defines the number of columns: if subsequent rows have
  fewer cells, they are padded with #f, if it has more, they are silently
  truncated.

  @racket[na] represents the value in the CSV file that represents the "not
  available" value in the data frame.  Strings @racket[equal?] to this value
  will be replaced by @racket[#f].  Alternatively, this can be a function
  which tests a string and returns @racket[#t] if the string represents a NA
  value

  When @racket[quoted-numbers?] is @racket[#t], all quoted values in the CSV
  file will be converted to numbers, if possible.  E.g. a value like "123"
  will be converted to the number 123 if @racket[quoted-numbers?] is
  @racket[#t], but will remain the string "123" if the parameter is
  @racket[#f].

}

@defproc[(df-write/csv (df data-frame?)
                       (output (or/c path-string? output-port?))
                       (#:start start exact-nonnegative-integer?)
                       (#:stop stop exact-nonnegative-integer?)
                       (series string?) ...)
                       any/c]{

  Write the data frame @racket[df] to @racket[output] which is either an
  output port or a string, in which case it is assumed to be a file name.  The
  series to be written out can be specified as the @racket[series] list.  If
  no @racket[series] are specified, all series in the data frame are written
  out as columns in an unspecified order.

  @racket[start] and @racket[stop] denote the beginning and end rows to be
  written out, by default all rows are written out.

}

@defproc[(df-read/sql (db connection?)
                      (query (or/c string? virtual-statement?))
                      (param any/c) ...)
                      data-frame?]{

  Create a data frame from the result of running @racket[query] on the
  database @racket[db] with the supplied list of parameters.  Each column from
  the result set will become a series in the data frame, @racket[sql-null]
  values will be converted to @racket[#f].

}

@section{Inspecting data series}

@defproc[(df-describe (df data-frame?)) any/c]{

  Print a nice description of @racket[df] to the @racket[current-output-port].
  This function is useful in interactive mode to quickly check the series and
  properties available in a data frame.

}

@section{Working With Properties}

A data frame can have arbitrary data attached to it the form of key-value
pairs, where the keys are symbols.  This is usefull for attaching additional
meta-data to data frames.  The functions allow working with properties.

@defproc[(df-property-names (df data-frame?)) (listof symbol?)]{

  Return the property names in the data frame @racket[df], as a list of
  symbols.  The names are returned in an unspecified order.

}

@defproc[(df-put-property! (df data-frame?) (key symbol?) (value any/c)) any/c]{

  Set the property @racket[key] to @racket[value] inside the data frame
  @racket[df].  If there is already a value for the property @racket[key], it
  is replaced.

}

@defproc[(df-get-property (df data-frame?)
                          (key symbol?)
                          (default any/c (lambda () #f)))
                          any/c]{

  Return the value for the property @racket[key] in the data frame
  @racket[df].  If there is no value for @racket[key], the @racket[default]
  function is called to return a value (the default just returns @racket[#f])

}

@defproc[(df-del-property! (df data-frame?)
                           (key symbol?))
                           any/c]{

  Delete the value for the property @racket[key] from the data frame
  @racket[df].  Does nothing if there is no value for the property
  @racket[key].

}

@section{NA Values}

Data series support the concept that a value "not available".  This is done
using a special value, usually @racket[#t], but separate for each data series.
The functions below allow working with ``NA'' values.  The NA value is
specified when the series is created using @racket[make-series].

@defproc[(df-count-na (df data-frame?) (series string?)) exact-nonnegative-integer?]{

  Return the number of ``NA'' values in the @racket[series].

}

@defproc[(df-is-na? (df data-frame?) (series string?) (value any/c)) boolean?]{

  Return @racket[#t] if @racket[value] is @racket[equal?] to the ``NA'' value
  in the @racket[series].  Each series in a data frame can have a different
  ``Not available'' value, but this value usually defaults to @racket[#f]

}

@defproc[(df-has-na? (df data-frame?) (series string?)) boolean?]{

  Return @racket[#t] if @racket[series] has any ``NA'' values.

}

@defproc[(df-has-non-na? (df data-frame?) (series string?)) boolean?]{

  Return @racket[#t] if @racket[series] has any values outside the ``NA''
  values.

}

@defproc[(df-na-value (df data-frame?) (series string?)) any/c]{

  Return the ``NA'' value for the @racket[series] in the data frame
  @racket[df].

}

@section{Get and Set Individual Values}

@deftogether[(
@defproc[(df-ref (df data-frame?) (position index/c) (series string?)) any/c]
@defproc[(df-ref* (df data-frame?) (position index/c) (series string?) ...) vector?])]{

  Return the value at @racket[position] for @racket[series] in the data frame
  @racket[df].  The second form allows referencing values from multiple
  series, and a vector containing the values is returned in this case.

}

@defproc[(df-set! (df data-frame?) (position index/c) (value any/c) (series string?)) any/c]{

  Update the value at @racket[position] in the @racket[series] to
  @racket[value].  The new value must keep the series sorted, if the series is
  sorted, and match the series contract, if a contract has been set for the
  series.

}

@section{Indexing And Row Lookup}

@deftogether[(@defproc[(df-add-index! (df data-frame?) (name string?) (series string?) (lt (-> any/c any/c boolean?)) (#:na-in-front? na-in-front? boolean? #f)) any/c]
             @defproc[(df-add-index*! (df data-frame?) (name string?) (series (listof string?)) (lt (listof (-> any/c any/c boolean?))) (#:na-in-front? na-in-front? boolean? #f)) any/c])]{

  Add a secondary index to the data frame @racket[df] named @racket[name] --
  if an index by that name already exists, it will be replaced.  A secondary
  index will allow fast lookups (see @racket[df-index-of] and
  @racket[df-lookup]) and iteration (see @racket[in-data-frame/by-index]) in
  the order defined by the ordering function.  Multiple indexes can be defined
  for a data frame for one or more columns and they will be used as needed.

  @racket[df-add-index!] will create an index on a single @racket[series] and
  use the @racket[lt] function for comparing elements, this function must
  provide a strict less-than ordering, and suitable values would be @racket[<]
  for numbers and @racket[string<?] for strings, although any function can be
  defined.

  The @racket[na-in-front?] determines where the ``NA'' values in the series
  are placed.  If it is @racket[#t], they are placed before all other values,
  otherwise they are placed at the end.

  The @racket[df-add-index*!] will define a multi-column index on all the
  @racket[series] specified as a list, the @racket[lt] parameter fot this
  function is a list of comparison functions, one for each columns. Such an
  index will sort by the first column, than, for all equal values in the first
  column, it will sort on the second column, and so on.

  A multi-column index is mosty used for defining a multi-column iteration
  order, however, such an index can still be used for fast lookup for elements
  in the first indexed series.

}

@defproc[(df-del-index! (df data-frame?) (name string?)) any/c]{

  Delete the index named @racket[name] from the data frame @racket[df]. Does
  nothing if an index by this name does not exist.

}

@defproc[(df-index-names (df data-frame?)) (listof string?)]{

  Return the list of index names defined for the data frame @racket[df].  The
  order of the index names is undedined.

}

@defproc[(df-index-series (df data-frame?) (name string?)) (listof string?)]{

  Return the list of series names indexed by the index @racket[name] in the
  data frame @racket[df].  The series names is returned in the order in which
  they are indexed.

}

@deftogether[(
@defproc[(df-index-of (df data-frame?) (series string?) (value any/c) (#:exact-match? exact-match? #f)) index/c]
@defproc[(df-index-of* (df data-frame?) (series string?) (#:exact-match? exact-match? #f) (value any/c) ...) (listof index/c)])]{

  Find the position of a @racket[value] or list of values in a @racket[series]
  of the data frame @racket[df].  Returns either a single value or a list of
  values.

  The series must either be sorted, see @racket[df-set-sorted!], or an index
  must be defined for it, see @racket[df-add-index!] and
  @racket[df-add-index*!], otherwise the calls will raise an error.

  @racket[exact-match?] defines what to do when the value(s) are not found in
  the data series.  If it is @racket[#t] and the value is not found, the
  functions return @racket[#f].

  If @racket[exact-match?] is @racket[#f], the @racket[value] need not be
  present in the @racket[series], in that case, the returned index is the
  position of the first element which comes after the @racket[value],
  according to the sort function.  This is the position where @racket[value]
  could be inserted and still keep the series sorted.  A value of 0 is
  returned if @racket[value] is less or equal than the first value of the
  series and a value of @racket[(df-row-count df)] is returned if the value is
  greater than all the values in @racket[series].

}

@defproc[(df-equal-range (df data-frame?) (series string?) (value any/c))
         (values index/c index/c)]{

  Finds the lower bound of appearance (inclusive) and upper bound of
  appearance (exclusive) of @racket[value], and return them respectively, in
  the data frame @racket[df]. This is useful for when a given series has
  multiple elements, and you want to find all of their occurrences. As the
  given series must be sorted, this is a range, and not a collection of
  indices.

  The series must be sorted (see @racket[df-set-sorted!]), or else this will
  error.

  The given @racket[value] need not be present in the @racket[series]. If this
  is the case, the lower bound and upper bound are the same and represent the
  position of the first element which comes before @racket[value], according
  to the sort function. This is the position in which the given value could be
  inserted and keep the series sorted.
}

@defproc[(df-all-indices-of (df data-frame?) (series string?) (value any/c)) (listof index/c)]{

  Return the list of positions where @racket[value] is found in the
  @racket[series] of the data frame @racket[df].  Returns an empty list if
  @racket[value] does not exist.

  The series must be either sorted, see @racket[df-set-sorted!], or have an
  index defined for it, see @racket[df-add-index!] and
  @racket[df-add-index*!], otherwise an error is reported.

}

@deftogether[(
@defproc[(df-lookup (df data-frame?)
                    (base-series string?)
                    (series (or/c string? (listof string?)))
                    (value any/c)
                    (#:exact-match? exact-match? #f))
                    any/c]
@defproc[(df-lookup* (df data-frame?)
                     (base-series string?)
                     (series (or/c string? (listof string?)))
                     (#:exact-match? exact-match? #f)
                     (value any/c) ...)
                     list?])]{

  Lookup the index for @racket[value] in @racket[base-series] and return the
  corresponding value in @racket[series].  if @racket[series] is a single
  string, a single value is returned, if it is a list of names, a list of
  values is returned.

  @racket[df-lookup*] allows looking up multiple values and will return a list
  of the corresponding values.

  These functions combine @racket[df-index-of] and @racket[df-ref] into a
  single function and has the same restrictions as @racket[df-index-of]: the
  series must either be sorted or an index defined for it.

  @racket[exact-match?] has the same meaning as for @racket[df-index-of]

}

@defproc[(df-lookup/interpolated (df data-frame?)
                                (base-series string?)
                                (series (or/c string? (listof string?)))
                                (value any/c)
                                (#:interpolate interpolate (-> real? any/c any/c any/c))
                                   (lambda (t v1 v2) (+ (* t v1) (* (- 1 t) v2))))
                               any/c]{

  Perform an interpolated lookup: same as @racket[df-lookup], but if
  @racket[value] is not found exactly in @racket[base-series], it's relative
  position is determined and it is used to interpolate values from the
  corresponding @racket[series].  This only works for sorted series, see
  @racket[df-set-sorted!].

  An interpolation function can be specified, if the default one is not
  sufficient.  This function is called once for each value resulting
  @racket[series] (i.e. it interpolates values one by one).

}

@section{Extracting Data}

@deftogether[(@defproc[(df-select (df data-frame?)
                                  (series string?)
                                  (#:filter filter (or/c #f (-> any/c any/c)) #f)
                                  (#:start start index/c 0)
                                  (#:stop stop index/c (df-row-count df)))
                       vector?]
             @defproc[(df-select* (df data-frame?)
                                  (#:filter filter (or/c #f (-> any/c any/c)) #f)
                                  (#:start start index/c 0)
                                  (#:stop stop index/c (df-row-count df))
                                  (series string?) ...) vector?])]{

  @racket[df-select] returns a vector with the values in the series
  @racket[series] from the data frame @racket[df], while @racket[df-select*]
  returns a vector where each element is a vector containing values from one
  ore more @racket[series] specified as an argument.

  @racket[start] and @racket[stop] indicate the first and one-before-last row
  to be selected. @racket[filter], when present, will filter values selected:
  only values for which the function returns @racket[#t] will be added to the
  resulting vector.

  If there is no @racket[filter] specified, the resulting vector will have
  @racket[(- stop start)] elements.  If there is a filter, the number of
  elements depends on how many are filtered out by this function.

}

@deftogether[(@defproc[(df-select/by-index (df data-frame?)
                                           (series string?)
                                           (#:index index-name string?)
                                           (#:from from any/c #f)
                                           (#:to to any/c #f)
                                           (#:filter filter (or/c #f (-> any/c any/c)) #f))
                       vector?]
             @defproc[(df-select/by-index* (df data-frame?)
                                           (series string?)
                                           (#:index index-name string?)
                                           (#:from from (listof any/c) #f)
                                           (#:to to (listof any/c) #f)
                                           (#:filter filter (or/c #f (-> any/c any/c)) #f))
                       vector?]
             @defproc[(df-select*/by-index (df data-frame?)
                                           (#:index index-name string?)
                                           (#:from from any/c #f)
                                           (#:to to any/c #f)
                                           (#:filter filter (or/c #f (-> any/c any/c)) #f)
                                           (series string?) ...)
                      vector?]
             @defproc[(df-select*/by-index* (df data-frame?)
                                            (#:index index-name string?)
                                            (#:from from (listof any/c) #f)
                                            (#:to to (listof any/c) #f)
                                            (#:filter filter (or/c #f (-> any/c any/c)) #f)
                                            (series string?) ...)
                      vector?]

             )]{

  Same as @racket[df-select], and @racket[df-select*], but these functions
  return elements in the order defined by the index @racket[index-name], and
  elements are returned between the rows defined by the first occurence of
  @racket[from] and the last occurence of @racket[to] in the indexed series
  (the indexed series can contain duplicates).

  @bold{HINT}: to select data from all rows where a colunm has a specified
  value, define an index for the column and select using the same value for
  both @racket[from] and @racket[to].

  A multi-column index can also be iterated using a single value, in which
  case only the first column of the index is used.  For example, if you have a
  data frame with @italic{Country}, @italic{City} and @italic{CityPopulation}
  series, you can define an index on @italic{Country} and
  @italic{CityPopulation}, than select @italic{City} and
  @italic{CityPopulation} on that index with a specific country as the
  @racket[from] and @racket[to] arguments.  This will return all cities in
  that country ordered by their population.

  The @bold{by-index*} versions of these functins allow specifying a
  multi-value key for a multivalue index.

}

@defproc[(valid-only (item any/c)) boolean?]{

  A small utility function that can be used as an argument to the
  @italic{filter} argument to the @italic{select} functions, to select only
  rows which have actual data (i.e. not NA values).  This function assumes
  that the NA value is always @racket[#f].

  The function returns @racket[#t] if all elements of @racket[item] (which can
  be a vector or a list) are not @racket[#f].

}

@section{Iterating Over Rows}

@deftogether[(@defproc[(df-map (df data-frame?)
                               (series (or/c string? (listof string?)))
                               (fn mapfn/c)
                               (#:start start index/c 0)
                               (#:stop stop index/c (df-row-count df)))
                       vector?]
             @defproc[(df-map/by-index (df data-frame?)
                                       (series (or/c string? (listof string?)))
                                       (fn mapfn/c)
                                       (#:index index-name string?)
                                       (#:from from any/c #f)
                                       (#:to to any/c #f))
                      vector?]
             @defproc[(df-map/by-index* (df data-frame?)
                                        (series (or/c string? (listof string?)))
                                        (fn mapfn/c)
                                        (#:index index-name string?)
                                        (#:from from (listof any/c) #f)
                                        (#:to to (listof any/c) #f))
                      vector?])]{

  Apply the function @racket[fn] over rows in the specified @racket[series]
  and return a vector of the values that @racket[fn] returns.

  @racket[fn] is a function of ether one or two arguments.  If @racket[fn] is
  a function with one argument, it is called with the values from all
  @racket[series] as a single vector.  If @racket[fn] is a function of two
  arguments, it is called with the current and previous set of values, as
  vectors (this allows calculating "delta" values).  I.e. @racket[fn] is
  invoked as @racket[(fn prev current)].  If @racket[fn] accepts two
  arguments, it will be invoked as @racket[(fn #f current)] for the first
  element of the iteration.

  @racket[df-map] will iterate over rows in the data frame between
  @racket[start] and @racket[stop] positions, while @racket[df-map/by-index]
  and @racket[df-map/by-index*] will iterate in the order defined by the index
  @racket[index-name] between @racket[from] and @racket[to] values in the
  indexed series.

  See @racket[df-select/by-index*] for a discution on the differneces between
  @bold{by-index} and @bold{by-index*} versions.

}

@deftogether[(@defproc[(df-for-each (df data-frame?)
                                    (series (or/c string? (listof string?)))
                                    (fn mapfn/c)
                                    (#:start start index/c 0)
                                    (#:stop stop index/c (df-row-count df)))
                       void]
             @defproc[(df-for-each/by-index (df data-frame?)
                                            (series (or/c string? (listof string?)))
                                            (fn mapfn/c)
                                            (#:index index-name string?)
                                            (#:from from any/c #f)
                                            (#:to to any/c #f))
                      void]
             @defproc[(df-for-each/by-index* (df data-frame?)
                                             (series (or/c string? (listof string?)))
                                             (fn mapfn/c)
                                             (#:index index-name string?)
                                             (#:from from (listof any/c) #f)
                                             (#:to to (listof any/c) #f))
                      void])]{

  Same as @racket[df-map] and its variants, but the result of calling
  @racket[fn] is discarded and the function returns nothing.

}

@deftogether[(@defproc[(df-fold (df data-frame?)
                                (series (or/c string? (listof string?)))
                                (init-value any/c)
                                (fn foldfn/c)
                                (#:start start index/c 0)
                                (#:stop stop index/c (df-row-count df)))
                       any/c]
             @defproc[(df-fold/by-index (df data-frame?)
                                        (series (or/c string? (listof string?)))
                                        (init-value any/c)
                                        (fn foldfn/c)
                                        (#:index index-name string?)
                                        (#:from from any/c #f)
                                        (#:to to any/c #f))
                      any/c]
             @defproc[(df-fold/by-index* (df data-frame?)
                                         (series (or/c string? (listof string?)))
                                         (init-value any/c)
                                         (fn foldfn/c)
                                         (#:index index-name string?)
                                         (#:from from (listof any/c) #f)
                                         (#:to to (listof any/c) #f))
                      any/c])]{

  Fold the function @racket[fn] over rows in the specified @racket[series].
  @racket[init-val] is the initial value for the fold operation.  The last
  value returned by @racket[fn] is returned by the folding function.

  @racket[fn] is a function of ether two or three arguments.  If @racket[fn]
  is a function with two arguments, it is called with the fold value plus the
  values from all @racket[series] is passed in as a single vector.  If
  @racket[fn] is a function of three arguments, it is called with the fold
  value plus the current and previous set of values, as vectors (this allows
  calculating "delta" values).  I.e. @racket[fn] is invoked as @racket[(fn val
  prev current)].  If @racket[fn] accepts two arguments, it will be invoked as
  @racket[(fn init-val #f current)] for the first element of the iteration.

  @racket[df-fold] will iterate over rows in the data frame between
  @racket[start] and @racket[stop] positions, while @racket[df-fold/by-index]
  and @racket[df-fold/by-index*] will iterate in the order defined by the
  index @racket[index-name] between @racket[from] and @racket[to] values in
  the indexed series.

  See @racket[df-select/by-index*] for a discution on the differneces between
  @bold{by-index} and @bold{by-index*} versions.

}

@deftogether[(@defproc[(in-data-frame (df data-frame?)
                                      (#:start start index/c 0)
                                      (#:stop stop index/c (df-row-count df))
                                      (series string?) ...)
                       sequence?]
             @defproc[(in-data-frame/as-list (df data-frame?)
                                             (#:start start index/c 0)
                                             (#:stop stop index/c (df-row-count df))
                                             (series string?) ...)
                      sequence?]
             @defproc[(in-data-frame/as-vector (df data-frame?)
                                               (#:start start index/c 0)
                                               (#:stop stop index/c (df-row-count df))
                                               (series string?) ...)
                      sequence?])]{

  Return a sequence that produces values from a list of @racket[series]
  between @racket[start] and @racket[stop] rows.  The sequence produces
  values, each one corresponding to one of the @racket[series].

  This is intended to be used in @racket[for] and related constructs to
  iterate over elements in the data frame:

@racketblock[
(for (([lat lon] (in-data-frame df "lat" "lon")))
  (printf "lat = ~a, lon = ~a~%" lat lon))]

  The @racket[in-data-frame/as-list] and @racket[in-data-frame/as-vector]
  variants work the same, but they produce a single value, a list or a vector
  containing a row of values from the @racket[series]:

@racketblock[
(for ((coord (in-data-frame/as-list df "lat" "lon")))
   (match-define (list lat lon) coord)
   (printf "lat = ~a, lon = ~a~%" lat lon))]

}

@deftogether[(@defproc[(in-data-frame/by-index (df data-frame?)
                                               (#:index index-name string?)
                                               (#:from from any/c #f)
                                               (#:to to any/c #f)
                                               (series string?) ...)
                       sequence?]
             @defproc[(in-data-frame/by-index/as-list (df data-frame?)
                                                      (#:index index-name string?)
                                                      (#:from from any/c #f)
                                                      (#:to to any/c #f)
                                                      (series string?) ...)
                      sequence?]
             @defproc[(in-data-frame/by-index/as-vector (df data-frame?)
                                                        (#:index index-name string?)
                                                        (#:from from any/c #f)
                                                        (#:to to any/c #f)
                                                        (series string?) ...)
                      sequence?]
             @defproc[(in-data-frame/by-index* (df data-frame?)
                                               (#:index index-name string?)
                                               (#:from from any/c #f)
                                               (#:to to any/c #f)
                                               (series string?) ...)
                       sequence?]
             @defproc[(in-data-frame/by-index*/as-list (df data-frame?)
                                                       (#:index index-name string?)
                                                       (#:from from any/c #f)
                                                       (#:to to any/c #f)
                                                       (series string?) ...)
                      sequence?]
             @defproc[(in-data-frame/by-index*/as-vector (df data-frame?)
                                                         (#:index index-name string?)
                                                         (#:from from any/c #f)
                                                         (#:to to any/c #f)
                                                         (series string?) ...)
                      sequence?])]{

  Same as the @racket[in-data-frame] constructs, but these iterate over the
  index @racket[index-name] between @racket[from] and @racket[to] values in
  the index.

  See @racket[df-select/by-index*] for a discution on the differneces between
  @bold{by-index} and @bold{by-index*} versions and on using @racket[from] and
  @racket[to] values.

}


@section{Statistics}

The following functions allow calculating statistics on data frame
series.  They build on top of the @racket[math/statistics] module.

@deftogether[(
@defproc[(df-set-default-weight-series! (df data-frame?) (series (or/c #f string?))) any/c]
@defproc[(df-get-default-weight-series (df data-frame?)) (or/c #f string?)])]{

Set or return the default weight series for statistics operations.
This series will be used as a weight series if none is specified for
@racket[df-statistics] or @racket[df-quantile].  Set it to #f for no
weight series to be used for statistics.

A weight series needs to be used when samples in the data frame don't
have equal weight.  For example, if a parameter (e.g. heart rate) is
recorded at variable intervals, simply averaging the values will not
produce an accurate average, if a timer series is also present, it can
be used as a weight series to produce a better average.

}

@defproc[(df-statistics (df data-frame?)
                        (series string?)
                        (#:weight-series weight-series string? (df-get-default-weight-series df))
                        (#:start start exact-nonnegative-integer? 0)
                        (#:stop stop exact-nonnegative-integer? (df-row-count df)))
                        (or/c #f statistics?)]{

Compute statistics for @racket[series] in the data frame @racket[df].
This calls @racket[update-statistics] for the values in the series.
The statistics computation will use weighting if a weight series is
defined for the data frame, see @racket[df-set-default-weight-series!].

}

@defproc[(df-quantile (df data-frame?)
                      (series string?)
                      (#:weight-series string? (df-get-default-weight-series df))
                      (#:less-than less-than (-> any/c any/c boolean?) <)
                      (qvalue (between/c 0 1)) ...)
                      (or/c #f (listof real?))]{

Return the quantiles for the @racket[series] in the data frame
@racket[df].  A list of quantiles is returned as specified by
@racket[qvalue], or if no quantiles are specified, the list @racket[(0
0.25 0.5 0.75 1)] is used. @racket[#:weight-series] has the usual meaning,
@racket[less-than] is the ordering function passed to the
@racket[quantile] function.

}

@section{Least Squares Fitting}

@defstruct[least-squares-fit ([type (or/c 'linear 'polynomial 'power 'exponential 'logarithmic)]
                              [coefficients (listof real?)]
                              [residual (or/c #f real?)]
                              [fn (-> real? real?)])]{

Return value for the @racket[df-least-squares-fit] function,
containing the fiting mode and coefficients for the function. The
structure can be applied directly as a procedure and acts as the fit
function.

}

@defproc[(df-least-squares-fit (df data-frame?) (xseries string?) (yseries string?)
         (#:start start exact-nonnegative-integer? 0)
         (#:stop stop exact-nonnegative-integer? (df-row-count df))
         (#:mode mode (or/c 'linear 'polynomial 'poly 'power 'exponential 'exp 'logarithmic 'log) 'linear)
         (#:polynomial-degree degree exact-nonnegative-integer? 2)
         (#:residual? residual? boolean? #f)
         (#:annealing? annealing? boolean? #f)
         (#:annealing-iterations iterations exact-nonnegative-integer? 500))
         least-squares-fit?]{

Return a best fit function for the @racket[xseries] and
@racket[yseries] in the data frame @racket[df]. This function returns
a @racket[least-squares-fit] structure instance.  The instance can be
applied directly as a function, being the best fit function for the
input data.

@racket[start] and @racket[stop] specify the start and end position in
the series, by default all values are considered for the fit.

@racket[mode] determines the type of the function being fitted and can
have one of the following values:

@itemize[

@item{@racket['linear] -- a function Y = a * X + b is fitted where 'a'
        and 'b' are fitted; this is equivalent of fitting a
        'polynomial of degree 1 (see below)}

@item{@racket['polynomial] or @racket['poly] -- a polynomial Y = a0 +
        a1 * X + a2 * X^2 + ... is fitted.  The degree of the
        polynomial is specified by the @racket[degree] parameter, by
        default this is 2.}

@item{@racket['exponential] or @racket['exp] -- a function of Y = a *
        e ^ (b * X) + c is fitted.  Note that this fit is not very
        good, and annealing needs to be used to improve it (see below)}

@item{@racket['logarithmic] or @racket['log] -- a function of type Y =
        a + b * ln(X) is fitted.  This will only return a "real" fit
        function (as opposed to an imaginary one) if all values in
        YSERIES are positive}

@item{@racket['power] -- a function of type Y = a * X ^ b is
        fitted. This will only return a "real" fit function (as
        opposed to an imaginary one) if all values in YSERIES are
        positive.  Note that this fit is not very good, and annealing
        needs to be used to improve it (see below)}

]

@racket[residual?] when @racket[#t] indicates that the residual value
is also returned in the `least-squares-fit` structure.  Setting it to
#f will avoid some unnecessary computations.

@racket[annealing?] when @racket[#t] indicates that the fit
coefficients should be further refined using the @racket[annealing]
function.  This is only used for @racket['exponential] or

@racket['power] fit functions as these ones do not produce "best fit"
coefficients -- I don't know why, I am not a mathematician, I only
used the formulas.  Using annealing will significantly improve the fit
for these functions, but will still not determine the best one.  Note
that the annealing algorithm is probabilistic, so applying it a second
time on the same arguments will produce a slightly different result.

@racket[iterations] represents the number of annealing iterations, see
the #:iterations parameter to the `annealing` function.

}

@section{Histograms and histogram plots}

@defproc[(df-histogram (df data-frame?)
                       (series string?)
                       (#:weight-series weight-series (or/c #f string?) (df-get-default-weight-series df))
                       (#:bucket-width bucket-width real? 1)
                       (#:trim-outliers trim-outliers (or/c #f (between/c 0 1)) #f)
                       (#:include-zeroes? include-zeroes? boolean? #t)
                       (#:as-percentage? as-percentage? boolean? #f)
                       (#:start start exact-nonnegative-integer?)
                       (#:stop stop exact-nonnegative-integer?))
                       (or/c #f histogram/c)]{

Create a histogram for @racket[series] from the data frame @racket[df] between
rows @racket[start] and @racket[stop] (which default to all the rows).  The
returned is a vector of values, each value is a vector of two values, the
sample and the rank of that sample.

@racket[weight-series] specifies the series to be used for weighting
the samples. By default, it it uses the @racket['weight] property
stored in the data-frame, see @racket[df-set-default-weight-series!].
Use @racket[#f] for no weighting, in this case, each sample will have
a weight of 1.

@racket[bucket-width] specifies the width of each histogram slot.
Samples in the data series are grouped together into slots, which are
from 0 to @racket[bucket-width], than from @racket[bucket-width] to
@racket[(* 2 bucket-width)] and so on.  The @racket[bucket-width]
value can be less than @racket[1.0].

@racket[trim-outliers] specifies to remove slots from both ends of the
histogram that contain less than the specified percentage of values.
When @racket[#f] on slots are trimmed.

@racket[include-zeroes?] specifies whether samples with a slot of 0
are included in the histogram or not.  Note that slot 0 contains
samples from 0 to @racket[bucket-width].

@racket[as-percentage?] determines if the data in the histogram
represents a percentage (all ranks add up to 100) or it is the rank of
each slot.

In the resulting histogram, samples that are numbers or strings will
be sorted.  In addition, if the samples are numbers, empty slots will
be created so that the buckets are also consecutive.

}

@defproc[(histogram-renderer (histogram histogram/c)
                             (#:color color any/c #f)
                             (#:skip skip real? (discrete-histogram-skip))
                             (#:x-min x-min real? 0)
                             (#:label label string? #f)
                             (#:blank-some-labels blank-some-labels? boolean? #t)
                             (#:x-value-formatter formatter (or/c #f (-> number? string?)) #f))
                             (treeof renderer2d?)]{

Create a histogram plot renderer from @racket[data], which is a
histogram created by @racket[df-histogram].

@racket[color] determines the color of the histogram bars.

@racket[label] specifies the label to use for this plot renderer.

@racket[skip] and @racket[x-min] are used to plot dual histograms, see
@racket[histogram-renderer/dual].

All the above arguments are sent directly to the
@racket[discrete-histogram]

@racket[blank-some-labels?], controls if some of the labels are
blanked out if the plot contains too many values, this can produce a
nicer looking plot.

@racket[formatter] controls how the histogram values are displayed. By
default, labels for the values are displayed with @racket[~a], but
this function can be used for custom formatter.  For example, if the
values in the histogram represent running pace, the formatter can
transform a value of @racket[300] into the label @racket["5:00"].

}

@defproc[(combine-histograms (h1 histogram/c) (h2 histogram/c)) combined-histogram/c]{

Combine two histograms produced by @racket[df-histogram] into a single
one.  The result of this function is intended to be passed to
@racket[histogram-renderer/dual].

}

@defproc[(histogram-renderer/dual (combined-histogram combined-histogram/c)
                                  (label1 string?)
                                  (label2 string?)
                                  (#:color1 color1 any/c #f)
                                  (#:color2 color2 any/c #f)
                                  (#:x-value-formatter formatter (or/c #f (-> number? string?)) #f))
                                  (treeof renderer2d?)]{

Create a plot renderer that shows two histograms, with each slot
side-by-side.  The histograms can be produced by @racket[df-histogram]
and combined by @racket[combined-histogram].

@racket[label1] and @racket[color1] represent the label and colors for
the first histogram, @racket[label2] and @racket[color2] represent the
label and colors to use for the second histogram.

@racket[formatter] controls how the histogram values are displayed. By
default, labels for the values are displayed with @racket[~a], but
this function can be used for custom formatter.  For example, if the
values in the histogram represent running pace, the formatter can
transform a value of @racket[300] into the label @racket["5:00"].

}

@defproc[(histogram-renderer/factors (histogram histogram/c)
                                     (factor-fn (-> real? symbol?))
                                     (factor-colors (listof (cons/c symbol? color/c)))
                                     (#:x-value-formatter formatter (or/c #f (-> number? string?)) #f))
                                     (treeof renderer2d?)]{

Create a histogram renderer where @racket[histogram] is split into
sections by @racket[factor-fn] and each section is colored according
to @racket[factor-colors].

@racket[formatter] controls how the histogram values are displayed. By
default, labels for the values are displayed with @racket[~a], but
this function can be used for custom formatter.  For example, if the
values in the histogram represent running pace, the formatter can
transform a value of @racket[300] into the label @racket["5:00"].

}

@section{GPX Files}

@defmodule[data-frame/gpx]

This module provides functions for reading and writing data frames using the
@hyperlink["https://en.wikipedia.org/wiki/GPS_Exchange_Format"]{GPS Exchange
Format (GPX)}.

@defproc[(df-read/gpx (input (or/c path-string? input-port?)))
                      data-frame?]{

Construct a data frame from the GPX document specified in @racket[input],
which is either an input port or a string, in which case it denotes an input
file.  The data frame will have one or more of the following series:

@itemize[
  @item{"lat" and "lon" series representing the latitude and longitude of each
        point}
  @item{"timestamp" series representing the UTC timestamp in seconds for each
        point.  The series will also be marked as sorted, if it is actually
        sorted}
  @item{"dst" representing a distance from the start.  If distance data is not
        present in the GPX file, this series will be calculated from the GPX
        coordiantes.  The series will be marked as sorted, if it is actually
        sorted}
  @item{"hr" representing heart rate measurements}
  @item{"cad" representing cadence measurements}
  @item{"pwr" representing power measurements, in watts}
  @item{"spd" representing the speed}]

The data frame will also have the following properties:

@itemize[

@item{a @racket['name] property containing the name of the track
segment, if this is present in the GPX file.}

@item{a @racket['waypoints] property containing a list of waypoints,
if they GPX track has any.  Each waypoint is represented as a list of
TIMESTAMP, LAT, LON, ELEVATION and NAME}

@item{a @racket['laps] property containing a list of timestamps
corresponding to each way point in the waypoint list -- the laps
property cannot be constructed correctly if the waypoints are missing
a timestamp property.}]

All the track segments in the GPX file will be concatenated.}

@defproc[(df-write/gpx (df data-frame?)
                       (output (or/c path-string? output-port?))
                       (#:name name (or/c #f string?) #f)
                       (#:extra-series extra-series (listof string?) '("hr" "cad" "pwr" "spd" "dst"))
                       (#:start start exact-nonnegative-integer? 0)
                       (#:stop stop exact-nonnegative-integer? (df-row-count df)))
                       any/c]{

Export the GPS track from the data frame @racket[df] to
@racket[output], which is either an output port or a string, in which
case it denotes a file name.

The data frame is expected to contain the "timestamp", "lat", "lon" series,
and optionally "alt" or "calt" (corrected altitude) series.  In addition to
these series, optional heart rate, cadence, speed, power and distance data can
also be written out by specifying a list of series names in
@racket[extra-series], series which don't exist will be silently discarded.
Series which exist, but we don't know how to write them out are also silently
discarded (e.g. no "gpxdata:" tag)

The entire GPS track is exported as a single track segment, unless
@racket[start] and @racket[stop] positions are specified, in which case only
data between these positions is exported (this can be used to export a subset
of the data)

The @racket[laps] property, if present, is assumed to contain a list
of timestamps and the positions corresponding to these timestamps are
exported as way points.

The name of the segment can be specified as the @racket[name]
parameter. If this is @racket[#f], the @racket['name] property in the
data frame is consulted, if that one is missing a default track name
is used.

}

@section{TCX Files}

@defmodule[data-frame/tcx]

This module provides functions for reading
@hyperlink["https://en.wikipedia.org/wiki/Training_Center_XML"]{Training
Center XML (TCX)} files into data frames.

@defproc[(df-read/tcx (input (or/c path-string? input-port?)))
         data-frame?]{

  Construct a data frame from the first activity in the TCX document specified
  in @racket[input], which is either an input port or a string, in which case
  it denotes an input file.  The data frame will have one or more of the
  following series:

  @itemize[
    @item{"lat" and "lon" series representing the latitude and longitude of
          each point}
    @item{"timestamp" series representing the UTC timestamp in seconds for
          each point.  The series will also be marked as sorted, if it is
          actually sorted}
    @item{"dst" representing a distance from the start.  If distance data is
          not present in the GPX file, this series will be calculated from the
          GPX coordiantes.  The series will be marked as sorted, if it is
          actually sorted}
    @item{"hr" representing heart rate measurements}
    @item{"cad" representing cadence measurements}
    @item{"pwr" representing power measurements, in watts}
    @item{"spd" representing the speed}]

  The data frame may also have the following properties (if they are present
  in the TCX document):

  @itemize[

    @item{@racket['unit-id] the serial number of the device which recorded
          the activity.}

    @item{@racket['product-id] the product id for the device that recorded the
          activity (indentifies the device type)}

    @item{@racket['sport] the sport for the activity.  This is a free form
          string, but TCX format usualy uses the strings "Running" for running
          activities and "Biking" for biking activities.}

    @item{a @racket['laps] property containing a list of timestamps
          corresponding to the start of each lap in the activity.}

  ]

}

@defproc[(df-read/tcx/multiple (input (or/c path-string? input-port?)))
         (listof data-frame?)]{

  Construct a list of data frames, one for each activtiy in the TCX document
  specified in @racket[input], which is either an input port or a string, in
  which case it denotes an input file.  See @racket[df-read/tcx] for the
  contents of each data frame object.

}
