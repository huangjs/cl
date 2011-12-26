; units.lsp              Gordon S. Novak Jr.           ; 22 May 03

; Software for conversion and simplification of Units of Measurement

; This version puts the units functions in a separate package.

; Copyright (c) 2003 Gordon S. Novak Jr. and The University of Texas at Austin.

; See the file gnu.license for the GNU General Public License.

; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software Foundation,
; Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
; gnu@gnu.org, http://www.gnu.org/, 617 542-5942, fax 617 542-2652.

; Floating-point numbers that are returned as conversion factors
; by this software may appear to have more digits of accuracy than are
; justified by the accuracy of the underlying conversion factors.

; Written by: Gordon S. Novak Jr., Department of Computer Sciences,
; University of Texas at Austin  78712.    novak@cs.utexas.edu
; http://www.cs.utexas.edu/users/novak/

; Thanks to Erik Enge (erik@nittin.net) for thorough testing and suggestions
; and to him and Kevin Rosenberg for making this a Debian package.

; History:
; converted from Interlisp " 1-MAR-82 17:36:34" {DSK}MEASUREMENT.LSP;8 15862
; adapted from ./physics/measu.lsp of 29 Aug 90 for use with GLISP compiler
; modified 03 June 93 for paper on units: replaced MKS by SI.
; 05 Jun 95; 15 Dec 95; 09 Mar 99; 17 Mar 99; 20 May 99; 03 May 01; 02 Apr 02
; 17 Apr 02; 08 Apr 03; 18 Apr 03; 22 May 03

; Use the following to import the basic functions to the package where used:
; (import '(units:glconvertunit units:glsimplifyunit units:glunitp
;           units:*glunitenable* units:glmultunits units:glsimplunit
;           units:gldivunits))

; To make the basic conversion functions have easier names, do:
; (setf (symbol-function 'convertu)  (symbol-function 'units:glconvertunit))
; (setf (symbol-function 'simplifyu) (symbol-function 'units:glsimplifyunit))
; (setf (symbol-function 'unitp)     (symbol-function 'units:glunitp))

; A unit expression is:
;   a number
;   a unit name
;   (* unit-exp1 unit-exp2 ...)
;   (/ unit-exp1 unit-exp2)
; A simplified unit expression will have at most one (/ ...) at the top.

; (setq *glunitenable* t) to enable mass -> weight, mass -> energy conversion
; *glunitmethod* is set to w2m m2w m2e e2m if this is done.

; Examples of unit conversion:  (glconvertunit <from> <to>)
; (glconvertunit 'mile 'foot)
; (glconvertunit 'kilogram 'lb)
; (glconvertunit '(/ (* atto parsec) (* micro fortnight)) '(/ inch second))
; (glconvertunit '(* acre foot) 'teaspoon)
; (glconvertunit '(/ (* 2000 kilo calorie) day) 'watt)    ; average human power
; (glconvertunit '(* 100 kgf 4 m) '(* kilo calorie)) ; calories to climb stairs
; the following require (setq *glunitenable* t)
; (glconvertunit 'kilogram 'lbf)          ; mass to weight
; (glconvertunit 'gram 'kilowatt-hour)    ; mass to energy

; Examples of unit simplification:
; (glsimplifyunit '(/ meter foot))
; (glsimplifyunit '(/ joule watt))
; (glsimplifyunit '(/ joule horsepower))
; (glsimplifyunit '(/ (* kilogram meter) (* second second)))
; (glsimplifyunit 'atm)
; (glsimplifyunit 'atm 'english)
; (glsimplifyunit '(/ (* amp second) volt))
; (glsimplifyunit '(/ (* newton meter) (* ampere second)))
; (glsimplifyunit '(/ (* volt volt) (* lbf (/ (* atto parsec) hour))))

; Example of unit conversion by GLISP compiler:
; (gldefun test (speed\:(units real (/ (* atto parsec) (* micro fortnight))))
;   (if (speed > '(q 55 mph)) (print "speeding")))

; The following two lines can be omitted in older GCL versions:
(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (defpackage :units (:use :common-lisp)))

(in-package :units)

(export '(glconvertunit glsimplifyunit glunitp *glunitenable*
	  glmultunits glsimplunit gldivunits))

(import '(CL-USER::SI CL-USER::CGS CL-USER::ENGLISH
 CL-USER::GLCONSTANTP CL-USER::GLCONSTANTVAL CL-USER::GLERROR
 CL-USER::GLISPCONSTANTFLG CL-USER::GLISPCONSTANTVAL CL-USER::GLCONSTVAL
 CL-USER::$ CL-USER::% CL-USER::A CL-USER::ABAMP CL-USER::ABAMPERE
 CL-USER::ABAMPERES CL-USER::ABCOUL CL-USER::ABCOULOMB CL-USER::ABCOULOMBS
 CL-USER::ABFARAD CL-USER::ABFARADS CL-USER::ABHENRY CL-USER::ABHENRYS
 CL-USER::ABOHM CL-USER::ABOHMS CL-USER::ABVOLT CL-USER::ABVOLTS
 CL-USER::ACRE CL-USER::ACRE-FEET CL-USER::ACRE-FOOT CL-USER::ACREFEET
 CL-USER::ACREFOOT CL-USER::ACRES CL-USER::AMP CL-USER::AMP-HOUR
 CL-USER::AMPERE CL-USER::AMPERE-HOUR CL-USER::AMPERE-HOURS
 CL-USER::AMPEREHOUR CL-USER::AMPEREHOURS CL-USER::AMPERES CL-USER::AMPS
 CL-USER::AMU CL-USER::ANGSTROM CL-USER::ANGSTROMS CL-USER::ARC-MINUTE
 CL-USER::ARC-MINUTES CL-USER::ARC-SECOND CL-USER::ARC-SECONDS CL-USER::ARCMIN
 CL-USER::ARCMINUTE CL-USER::ARCMINUTES CL-USER::ARCSEC CL-USER::ARCSECOND
 CL-USER::ARCSECONDS CL-USER::ARE CL-USER::ARES CL-USER::ASTRONOMICAL-UNIT
 CL-USER::ATM CL-USER::ATMOSPHERE CL-USER::ATOMIC-MASS-UNIT
 CL-USER::ATOMIC-MASS-UNITS CL-USER::ATTO CL-USER::ATTO- CL-USER::ATTOSEC
 CL-USER::ATTOSECOND CL-USER::ATTOSECONDS CL-USER::AU CL-USER::BAR
 CL-USER::BARN CL-USER::BARNS CL-USER::BARS CL-USER::BECQUEREL
 CL-USER::BILLION CL-USER::BILLIONTH CL-USER::BQ CL-USER::BRITISH-THERMAL-UNIT
 CL-USER::BRITISH-THERMAL-UNITS CL-USER::BRITISHTHERMALUNIT
 CL-USER::BRITISHTHERMALUNITS CL-USER::BTU CL-USER::BTUS CL-USER::BUSHEL
 CL-USER::BUSHELS CL-USER::CAL CL-USER::CALORIE CL-USER::CALORIES
 CL-USER::CANDELA CL-USER::CANDELAS CL-USER::CC CL-USER::CD CL-USER::CENTI
 CL-USER::CENTI- CL-USER::CENTIGRAM CL-USER::CENTIGRAMS CL-USER::CENTILITER
 CL-USER::CENTILITERS CL-USER::CENTILITRE CL-USER::CENTIMETER
 CL-USER::CENTIMETER-CUBED CL-USER::CENTIMETER-PER-SECOND
 CL-USER::CENTIMETER-PER-SECOND-SQUARED CL-USER::CENTIMETER-SQUARED
 CL-USER::CENTIMETERCUBED CL-USER::CENTIMETERS CL-USER::CENTIMETERS-CUBED
 CL-USER::CENTIMETERS-PER-SECOND CL-USER::CENTIMETERS-PER-SECOND-SQUARED
 CL-USER::CENTIMETERS-SQUARED CL-USER::CENTIMETERSCUBED
 CL-USER::CENTIMETERSQUARED CL-USER::CENTIMETRE CL-USER::CENTURIES
 CL-USER::CENTURY CL-USER::CG CL-USER::CL CL-USER::CM CL-USER::CM/S
 CL-USER::CM/S/S CL-USER::CM/S2 CL-USER::CM/SEC CL-USER::CM/SEC/SEC
 CL-USER::CM/SEC2 CL-USER::CM^2 CL-USER::CM^3 CL-USER::COUL
 CL-USER::COULOMB CL-USER::COULOMBS CL-USER::CUBIC-CENTIMETER
 CL-USER::CUBIC-CENTIMETERS CL-USER::CUBIC-CENTIMETRE CL-USER::CUBIC-DECIMETER
 CL-USER::CUBIC-DECIMETERS CL-USER::CUBIC-FEET CL-USER::CUBIC-FOOT
 CL-USER::CUBIC-INCH CL-USER::CUBIC-INCHES CL-USER::CUBIC-KILOMETER
 CL-USER::CUBIC-KILOMETERS CL-USER::CUBIC-KILOMETRE CL-USER::CUBIC-METER
 CL-USER::CUBIC-METERS CL-USER::CUBIC-MICRON CL-USER::CUBIC-MICRONS
 CL-USER::CUBIC-MILE CL-USER::CUBIC-MILES CL-USER::CUBIC-MILLIMETER
 CL-USER::CUBIC-MILLIMETERS CL-USER::CUBIC-MILLIMETRE CL-USER::CUBIC-YARD
 CL-USER::CUBIC-YARDS CL-USER::CUP CL-USER::CUPS CL-USER::CURIE
 CL-USER::CURIES CL-USER::DAG CL-USER::DAM CL-USER::DAY CL-USER::DAYS
 CL-USER::DECA CL-USER::DECA- CL-USER::DECAGRAM CL-USER::DECAGRAMS
 CL-USER::DECALITER CL-USER::DECALITERS CL-USER::DECALITRE CL-USER::DECAMETER
 CL-USER::DECAMETERS CL-USER::DECAMETRE CL-USER::DECI CL-USER::DECI-
 CL-USER::DECIGRAM CL-USER::DECIGRAMS CL-USER::DECILITER
 CL-USER::DECILITERS CL-USER::DECILITRE CL-USER::DECIMETER
 CL-USER::DECIMETERS CL-USER::DECIMETRE CL-USER::DEG CL-USER::DEGREE
 CL-USER::DEGREE-KELVIN CL-USER::DEGREE-RANKINE CL-USER::DEGREES
 CL-USER::DEKA CL-USER::DEKA- CL-USER::DEKAGRAM CL-USER::DEKAGRAMS
 CL-USER::DEKALITER CL-USER::DEKALITERS CL-USER::DEKALITRE
 CL-USER::DEKAMETER CL-USER::DEKAMETERS CL-USER::DG CL-USER::DL
 CL-USER::DM CL-USER::DOLLAR CL-USER::DOLLARS CL-USER::DOZEN
 CL-USER::DRAM CL-USER::DRAMS CL-USER::DYNE CL-USER::DYNES
 CL-USER::DYNES-PER-SQUARE-CENTIMETER CL-USER::EARTH-GRAVITY
 CL-USER::EARTH-MASS CL-USER::ELECTRON-VOLT CL-USER::ELECTRON-VOLTS
 CL-USER::ELECTRONVOLT CL-USER::ELECTRONVOLTS CL-USER::ERG CL-USER::ERGS
 CL-USER::EV CL-USER::EXA CL-USER::EXA- CL-USER::FARAD CL-USER::FARADS
 CL-USER::FATHOM CL-USER::FATHOMS CL-USER::FEET CL-USER::FEET-PER-SECOND
 CL-USER::FEET-PER-SECOND-SQUARED CL-USER::FEET-SQUARED CL-USER::FEETCUBED
 CL-USER::FEETSQUARED CL-USER::FEMTO CL-USER::FEMTO- CL-USER::FEMTOSEC
 CL-USER::FEMTOSECOND CL-USER::FEMTOSECONDS CL-USER::FIFTH CL-USER::FIFTHS
 CL-USER::FLOZ CL-USER::FLUID-OUNCE CL-USER::FLUID-OUNCES
 CL-USER::FLUIDOUNCE CL-USER::FLUIDOUNCES CL-USER::FLUIDRAM CL-USER::FLUIDRAMS
 CL-USER::FOOT CL-USER::FOOT-CUBED CL-USER::FOOT-PER-SECOND
 CL-USER::FOOT-PER-SECOND-SQUARED CL-USER::FOOT-POUND CL-USER::FOOT-POUNDS
 CL-USER::FOOT-SQUARED CL-USER::FOOT/SEC CL-USER::FOOT/SEC/SEC
 CL-USER::FOOT/SECOND CL-USER::FOOT/SECOND/SECOND CL-USER::FOOTCUBED
 CL-USER::FOOTPOUND CL-USER::FOOTPOUNDS CL-USER::FOOTSQUARED
 CL-USER::FORTNIGHT CL-USER::FORTNIGHTS CL-USER::FPS CL-USER::FT
 CL-USER::FT-LB CL-USER::FT/S CL-USER::FT/S/S CL-USER::FT/S2
 CL-USER::FT/SEC CL-USER::FT/SEC/SEC CL-USER::FT/SEC2 CL-USER::FT/SECOND
 CL-USER::FTLB CL-USER::FT^2 CL-USER::FT^3 CL-USER::FURLONG CL-USER::FURLONGS
 CL-USER::GAL CL-USER::GALLON CL-USER::GALLONS CL-USER::GAUSS CL-USER::GEV
 CL-USER::GHZ CL-USER::GIGA CL-USER::GIGA- CL-USER::GIGA-ELECTRON-VOLTS
 CL-USER::GIGA-WATT CL-USER::GIGA-WATTS CL-USER::GIGAHERTZ CL-USER::GIGAWATT
 CL-USER::GIGAWATTS CL-USER::GILL CL-USER::GILLS CL-USER::GM CL-USER::GRAIN
 CL-USER::GRAINS CL-USER::GRAM CL-USER::GRAM-FORCE CL-USER::GRAM-WEIGHT
 CL-USER::GRAMS CL-USER::GRAY CL-USER::GROSS CL-USER::GW CL-USER::GY
 CL-USER::HECTARE CL-USER::HECTARES CL-USER::HECTO CL-USER::HECTO-
 CL-USER::HECTOGRAM CL-USER::HECTOGRAMS CL-USER::HECTOLITER
 CL-USER::HECTOLITERS CL-USER::HECTOLITRE CL-USER::HECTOMETER
 CL-USER::HECTOMETERS CL-USER::HECTOMETRE CL-USER::HENRY CL-USER::HENRYS
 CL-USER::HERTZ CL-USER::HG CL-USER::HM CL-USER::HORSEPOWER
 CL-USER::HORSEPOWER-HOUR CL-USER::HOUR CL-USER::HOURS CL-USER::HP
 CL-USER::HP-HOUR CL-USER::HR CL-USER::HUNDRED CL-USER::HUNDREDTH
 CL-USER::HUNDREDWEIGHT CL-USER::HUNDREDWEIGHTS CL-USER::HZ CL-USER::IN
 CL-USER::INCH CL-USER::INCH-CUBED CL-USER::INCH-SQUARED CL-USER::INCHCUBED
 CL-USER::INCHES CL-USER::INCHES-SQUARED CL-USER::INCHESCUBED
 CL-USER::INCHSQUARED CL-USER::IN^2 CL-USER::IN^3 CL-USER::J
 CL-USER::JOULE CL-USER::JOULES CL-USER::K CL-USER::KCAL
 CL-USER::KELVIN CL-USER::KELVINS CL-USER::KG CL-USER::KGF
 CL-USER::KHZ CL-USER::KILO CL-USER::KILO- CL-USER::KILO-CALORIE
 CL-USER::KILO-CALORIES CL-USER::KILO-PASCAL CL-USER::KILOCALORIE
 CL-USER::KILOGRAM CL-USER::KILOGRAM-FORCE CL-USER::KILOGRAM-WEIGHT
 CL-USER::KILOGRAMS CL-USER::KILOHERTZ CL-USER::KILOHM CL-USER::KILOHMS
 CL-USER::KILOLITER CL-USER::KILOLITERS CL-USER::KILOLITRE CL-USER::KILOMETER
 CL-USER::KILOMETER-CUBED CL-USER::KILOMETER-PER-HOUR
 CL-USER::KILOMETER-PER-SECOND CL-USER::KILOMETER-SQUARED
 CL-USER::KILOMETERCUBED CL-USER::KILOMETERS CL-USER::KILOMETERS-CUBED
 CL-USER::KILOMETERS-PER-HOUR CL-USER::KILOMETERS-PER-SECOND
 CL-USER::KILOMETERS-SQUARED CL-USER::KILOMETERSCUBED
 CL-USER::KILOMETERSQUARED CL-USER::KILOMETRE CL-USER::KILOPASCAL
 CL-USER::KILOPASCALS CL-USER::KILOWATT CL-USER::KILOWATT-HOUR
 CL-USER::KILOWATT-HOURS CL-USER::KILOWATTHOUR CL-USER::KILOWATTHOURS
 CL-USER::KILOWATTS CL-USER::KM CL-USER::KM^2 CL-USER::KM^3 CL-USER::KNOT
 CL-USER::KNOTS CL-USER::KPA CL-USER::KPH CL-USER::KPS CL-USER::KW
 CL-USER::KWH CL-USER::L CL-USER::LB CL-USER::LBF CL-USER::LBS
 CL-USER::LIGHT-YEAR CL-USER::LIGHT-YEARS CL-USER::LIGHTYEAR
 CL-USER::LIGHTYEARS CL-USER::LITER CL-USER::LITERS CL-USER::LITRE
 CL-USER::LONG-TON CL-USER::LONG-TONS CL-USER::LY CL-USER::M CL-USER::M/S
 CL-USER::M/S/S CL-USER::M/S2 CL-USER::M/SEC CL-USER::M/SEC/SEC
 CL-USER::M/SEC2 CL-USER::M/SECOND CL-USER::M/SECOND/SECOND CL-USER::MA
 CL-USER::MAXWELL CL-USER::MAXWELLS CL-USER::MEGA CL-USER::MEGA-
 CL-USER::MEGA-ELECTRON-VOLTS CL-USER::MEGA-WATT CL-USER::MEGA-WATTS
 CL-USER::MEGAHERTZ CL-USER::MEGAWATT CL-USER::MEGAWATTS CL-USER::MEGOHM
 CL-USER::MEGOHMS CL-USER::METER CL-USER::METER-CUBED CL-USER::METER-PER-SECOND
 CL-USER::METER-SQUARED CL-USER::METER/SEC CL-USER::METER/SEC/SEC
 CL-USER::METER/SECOND CL-USER::METER/SECOND/SECOND CL-USER::METERCUBED
 CL-USER::METERS CL-USER::METERS-CUBED CL-USER::METERS-PER-SECOND
 CL-USER::METERS-PER-SECOND-SQUARED CL-USER::METERS-SQUARED
 CL-USER::METERSQUARED CL-USER::METRE CL-USER::METRIC-TON CL-USER::METRIC-TONS
 CL-USER::MEV CL-USER::MG CL-USER::MH CL-USER::MHO CL-USER::MHZ CL-USER::MI
 CL-USER::MICRO CL-USER::MICRO- CL-USER::MICRO-METER CL-USER::MICRO-METERS
 CL-USER::MICRO-WATT CL-USER::MICRO-WATTS CL-USER::MICROAMP
 CL-USER::MICROAMPERE CL-USER::MICROAMPERES CL-USER::MICROAMPS
 CL-USER::MICROFARAD CL-USER::MICROFARADS CL-USER::MICROGRAM
 CL-USER::MICROGRAMS CL-USER::MICROHENRY CL-USER::MICROHENRYS
 CL-USER::MICROMETER CL-USER::MICROMETER-SQUARED CL-USER::MICROMETERS
 CL-USER::MICROMETERS-SQUARED CL-USER::MICROMETERSQUARED CL-USER::MICROMETRE
 CL-USER::MICRON CL-USER::MICRON-CUBED CL-USER::MICRON-SQUARED
 CL-USER::MICRONS CL-USER::MICRONS-CUBED CL-USER::MICRONS-SQUARED
 CL-USER::MICRONSQUARED CL-USER::MICROSEC CL-USER::MICROSECOND
 CL-USER::MICROSECONDS CL-USER::MICROVOLT CL-USER::MICROVOLTS
 CL-USER::MICROWATT CL-USER::MIL CL-USER::MILE CL-USER::MILE-CUBED
 CL-USER::MILE-PER-HOUR CL-USER::MILE-PER-SECOND CL-USER::MILE-SQUARED
 CL-USER::MILES CL-USER::MILES-CUBED CL-USER::MILES-PER-HOUR
 CL-USER::MILES-PER-SECOND CL-USER::MILES-SQUARED CL-USER::MILESQUARED
 CL-USER::MILE^2 CL-USER::MILE^3 CL-USER::MILLI CL-USER::MILLI-
 CL-USER::MILLI-WATT CL-USER::MILLI-WATTS CL-USER::MILLIAMP
 CL-USER::MILLIAMPERE CL-USER::MILLIAMPS CL-USER::MILLIBAR
 CL-USER::MILLIBARS CL-USER::MILLIGAUSS CL-USER::MILLIGRAM CL-USER::MILLIGRAMS
 CL-USER::MILLIHENRY CL-USER::MILLIHENRYS CL-USER::MILLILITER
 CL-USER::MILLILITERS CL-USER::MILLILITRE CL-USER::MILLIMETER
 CL-USER::MILLIMETER-CUBED CL-USER::MILLIMETER-SQUARED
 CL-USER::MILLIMETERCUBED CL-USER::MILLIMETERS CL-USER::MILLIMETERS-CUBED
 CL-USER::MILLIMETERS-SQUARED CL-USER::MILLIMETERSCUBED
 CL-USER::MILLIMETERSQUARED CL-USER::MILLIMETRE CL-USER::MILLION
 CL-USER::MILLIONTH CL-USER::MILLISEC CL-USER::MILLISECOND
 CL-USER::MILLISECONDS CL-USER::MILLIVOLT CL-USER::MILLIVOLTS
 CL-USER::MILLIWATT CL-USER::MILS CL-USER::MIN CL-USER::MINIM
 CL-USER::MINIMS CL-USER::MINUTE CL-USER::MINUTES CL-USER::ML CL-USER::MM
 CL-USER::MM^2 CL-USER::MM^3 CL-USER::MOL CL-USER::MOLE
 CL-USER::MOLES CL-USER::MON CL-USER::MONTH CL-USER::MONTHS CL-USER::MPH
 CL-USER::MS CL-USER::MSEC CL-USER::MV CL-USER::MW
 CL-USER::M^2 CL-USER::M^3 CL-USER::NANO CL-USER::NANO-
 CL-USER::NANOMETER CL-USER::NANOMETERS CL-USER::NANOMETRE
 CL-USER::NANOSEC CL-USER::NANOSECOND CL-USER::NANOSECONDS
 CL-USER::NAUTICAL-MILE CL-USER::NAUTICAL-MILES CL-USER::NAUTICALMILE
 CL-USER::NAUTICALMILES CL-USER::NEWTON CL-USER::NEWTONS CL-USER::NM
 CL-USER::NS CL-USER::NSEC CL-USER::NT CL-USER::OHM CL-USER::OHMS
 CL-USER::OUNCE CL-USER::OUNCE-FORCE CL-USER::OUNCE-TROY CL-USER::OUNCES
 CL-USER::OUNCES-TROY CL-USER::OZ CL-USER::PA CL-USER::PARSEC CL-USER::PARSECS
 CL-USER::PASCAL CL-USER::PECK CL-USER::PECKS CL-USER::PENNYWEIGHT
 CL-USER::PENNYWEIGHTS CL-USER::PERCENT CL-USER::PETA
 CL-USER::PETA- CL-USER::PF CL-USER::PI
 CL-USER::PICO CL-USER::PICO- CL-USER::PICOFARAD CL-USER::PICOFARADS
 CL-USER::PICOSEC CL-USER::PICOSECOND CL-USER::PICOSECONDS CL-USER::PINT
 CL-USER::PINTS CL-USER::POUND CL-USER::POUND-FORCE
 CL-USER::POUNDS CL-USER::POUNDS-PER-SQUARE-INCH CL-USER::PS
 CL-USER::PSEC CL-USER::PSI CL-USER::PT CL-USER::QT CL-USER::QUADRILLION
 CL-USER::QUADRILLIONTH CL-USER::QUART CL-USER::QUARTS CL-USER::QUINTILLION
 CL-USER::QUINTILLIONTH CL-USER::RAD CL-USER::RADIAN CL-USER::RADIANS
 CL-USER::RANKINE CL-USER::REM CL-USER::ROD CL-USER::RODS CL-USER::S
 CL-USER::SCRUPLE CL-USER::SCRUPLES CL-USER::SEC CL-USER::SECOND
 CL-USER::SECONDS CL-USER::SECS CL-USER::SHORT-TON CL-USER::SHORT-TONS
 CL-USER::SIEMENS CL-USER::SIEVERT CL-USER::SLUG CL-USER::SLUGS
 CL-USER::SPEED-OF-LIGHT CL-USER::SQUARE-CENTIMETER
 CL-USER::SQUARE-CENTIMETERS CL-USER::SQUARE-FEET CL-USER::SQUARE-FOOT
 CL-USER::SQUARE-INCH CL-USER::SQUARE-INCHES CL-USER::SQUARE-KILOMETER
 CL-USER::SQUARE-KILOMETERS CL-USER::SQUARE-METER CL-USER::SQUARE-METERS
 CL-USER::SQUARE-MICRON CL-USER::SQUARE-MICRONS CL-USER::SQUARE-MILE
 CL-USER::SQUARE-MILES CL-USER::SQUARE-MILLIMETER
 CL-USER::SQUARE-MILLIMETERS CL-USER::SQUARE-YARD CL-USER::SQUARE-YARDS
 CL-USER::SR CL-USER::STATAMP CL-USER::STATAMPERE CL-USER::STATAMPERES
 CL-USER::STATCOUL CL-USER::STATCOULOMB CL-USER::STATCOULOMBS
 CL-USER::STATFARAD CL-USER::STATFARADS CL-USER::STATHENRY CL-USER::STATHENRYS
 CL-USER::STATOHM CL-USER::STATOHMS CL-USER::STATVOLT CL-USER::STATVOLTS
 CL-USER::STERADIAN CL-USER::STERADIANS CL-USER::SV CL-USER::TABLESPOON
 CL-USER::TABLESPOONS CL-USER::TBSP CL-USER::TEASPOON CL-USER::TEASPOONS
 CL-USER::TEN CL-USER::TENTH CL-USER::TERA CL-USER::TERA-
 CL-USER::TERA-ELECTRON-VOLTS CL-USER::TERAHERTZ CL-USER::TESLA CL-USER::TESLAS
 CL-USER::TEV CL-USER::THOUSAND CL-USER::THOUSANDTH CL-USER::THZ
 CL-USER::TON CL-USER::TONNE CL-USER::TONNES CL-USER::TONS CL-USER::TORR
 CL-USER::TRILLION CL-USER::TRILLIONTH CL-USER::TROY-OUNCE CL-USER::TROY-OUNCES
 CL-USER::TROY-POUND CL-USER::TROY-POUNDS CL-USER::TSP CL-USER::UA CL-USER::UF
 CL-USER::UG CL-USER::UH CL-USER::UM CL-USER::UM^2
 CL-USER::UNITY CL-USER::US CL-USER::USEC CL-USER::UV CL-USER::UW CL-USER::V
 CL-USER::VOLT CL-USER::VOLTS CL-USER::W CL-USER::WATT CL-USER::WATT-HOUR
 CL-USER::WATT-HOURS CL-USER::WATTHOUR CL-USER::WATTHOURS
 CL-USER::WATTS CL-USER::WB CL-USER::WEBER CL-USER::WEBERS
 CL-USER::WEEK CL-USER::WEEKS CL-USER::WK CL-USER::YARD CL-USER::YARD-CUBED
 CL-USER::YARD-SQUARED CL-USER::YARDCUBED CL-USER::YARDS CL-USER::YARDS-CUBED
 CL-USER::YARDS-SQUARED CL-USER::YARDSCUBED CL-USER::YARDSQUARED
 CL-USER::YARD^2 CL-USER::YD CL-USER::YD^3 CL-USER::YEAR CL-USER::YEARS
 CL-USER::YOCTO CL-USER::YOCTO- CL-USER::YOTTA CL-USER::YOTTA- CL-USER::YR
 CL-USER::ZEPTO CL-USER::ZEPTO- CL-USER::ZETTA CL-USER::ZETTA-))

(defmacro quotep (x) `(and (consp ,x) (eq (car ,x) 'quote)))

(defmacro glispconstantflg    (x) `(get ,x 'glispconstantflg))
(defmacro glispconstantval    (x) `(get ,x 'glispconstantval))

; 28 Apr 94
; Cube root
; returns a negative real root for a negative argument.
(defun cbrt (x)
  (and (numberp x) (if (>= x 0) (expt x 1/3) (- (expt (- x) 1/3)))))

; modified version for stand-alone use with units
; 27 Mar 89; 06 Jun 90; 20 May 93; 03 Jan 95; 18 Apr 03
(defun glerror (fn msgstr &rest args)
  (format t "error detected by ~A~%" fn)
  (apply #'format (cons t (cons msgstr args)))
  (terpri) )

; modified version for stand-alone use with units
; 15-Feb-89; 05 Apr 90; 12 Sep 91; 18 Sep 91; 19 Sep 91; 17 Jan 92; 03 Nov 92
; 10 Nov 95; 26 Jul 96; 18 Apr 03
; Get the value of a compile-time constant 
(defun glconstval (x)
  (cond ((or (null x)
	     (eq x t)
	     (numberp x)
	     (characterp x)
	     (stringp x))
	  x)
	((and (symbolp x) (constantp x)) (eval x))
	((quotep x) (cadr x))
	((and (symbolp x)
	      (glispconstantflg x))
	  (glispconstantval x))
	(t (error "NOMSG"))))

(defmacro gldimension     (x) `(get ,x 'gldimension))
(defmacro glunittype      (x) `(get ,x 'glunittype))
(defmacro glsiconversion  (x) `(get ,x 'glsiconversion))
(defmacro glabbreviations (x) `(get ,x 'glabbreviations))
(defmacro glexpansion     (x) `(get ,x 'glexpansion))
(defmacro glactualunit    (x) `(get ,x 'glactualunit))
(defmacro glstdunits      (x) `(get ,x 'glstdunits))
(defmacro glsystemunits   (x) `(get ,x 'glsystemunits))
(defmacro glactualu       (x) `(or (glactualunit ,x) ,x))
(defmacro glunitsys       (x) `(get ,x 'glunitsys))

(defvar *gldimsizes* (make-array 8 :initial-contents
				   '(20 20 20 10 10 10 10 10)))
(defvar *gldimvals*  (make-array 8))
(defvar *gldimbias*  0)

; Test X to see if it represents a compile-time constant value.
; If undefined, define a simplified version.
(or (fboundp 'glconstantp)
    (eval '(defun glconstantp (x) (constantp x))))

; Get the value of a compile-time constant 
; If undefined, define a simplified version.
(or (fboundp 'glconstantval)
    (eval '(defun glconstantval (x) (if (constantp x) (eval x)))))

(or (fboundp 'while)
    (eval '(defmacro while (test &rest forms)
             `(loop (unless ,test (return)) ,@forms) ) ))

; 03 Jun 93; 16 Jul 93; 11 Nov 94
; Initialize factors for use in computing dimensions
(defun gldiminit ()
  (let ((f 1) (del 1) (bias 0))
    (dotimes (i 8)
      (setq f (* f del))
      (setf (aref *gldimvals* i) f)
      (setq del (aref *gldimsizes* i))
      (incf bias (* f (truncate del 2))) )
    (setq *gldimbias* bias) ))

(gldiminit)
  
; 04 Nov 92; 03 Jun 93; 16 Jul 93
; Convert a dimension list, which is a list of integers representing
; the powers of (mass length time charge), to an integer.
(defun gldimtoint (l)
  (let ((n 0))
    (dotimes (i 8) (incf n (* (pop l) (aref *gldimvals* i))))
    n))

; 03 Jun 93; 16 Jul 93; 26 Jul 93
; Convert a dimension integer to a dimension list
(defun glinttodim (n)
  (let ((m (+ n *gldimbias*)) l sz mm)
    (dotimes (i 8)
      (setq sz (aref *gldimsizes* i))
      (setq mm (truncate m sz))
      (push (- (- m (* mm sz)) (truncate sz 2))
	    l)
      (setq m mm))
    (nreverse l) ))

(dolist (pair '((length          1 0 0 0 0 0 0 0)
		(time            0 1 0 0 0 0 0 0)
		(temperature     0 0 1 0 0 0 0 0)
                (mass            0 0 0 1 0 0 0 0)
		(current         0 0 0 0 1 0 0 0)
		(substance       0 0 0 0 0 1 0 0)
		(luminosity      0 0 0 0 0 0 1 0)
		(money           0 0 0 0 0 0 0 1)
		(dimensionless   0 0 0 0 0 0 0 0)) )
        (setf (gldimension (car pair)) (gldimtoint (cdr pair))) )

(defvar *glunitenable* nil)  ; t to enable mass/weight, mass/energy conversions
(defvar *glunitmethod* nil)  ; which of above was used
(defvar *gldimweighttomass* (gldimtoint '( 1 -2 0 0 0 0 0 0)))
(defvar *gldimmasstoweight* (gldimtoint '(-1  2 0 0 0 0 0 0)))
(defvar *gldimmasstoenergy* (gldimtoint '(-2  2 0 0 0 0 0 0)))
(defvar *gldimenergytomass* (gldimtoint '( 2 -2 0 0 0 0 0 0)))
(defvar *gldimtounittype*   nil)
(defvar *glunkunits*   nil)  ; unknown units
(defvar *glunitdimerror* nil) ; error in dimensionality

; 04 Nov 92
; Define simple measurement units.  Args are unit type and a list of units.
; Each unit is specified by a list: (unit-name glsiconversion synonyms)
(defun gldefsimpleunits (unit-type units)
  (dolist (unitlist units)
    (gldefunit (first unitlist) unit-type (second unitlist) (third unitlist))))

; 04 Nov 92
(defun gldefunit (unit unit-type factor abbrevs)
  (if (symbolp unit-type)
      (setf (glunittype unit) unit-type))
  (setf (gldimension unit)
	(if (symbolp unit-type)
	    (gldimension unit-type)
	    unit-type))
  (setf (glsiconversion unit) factor)
  (if abbrevs (setf (glabbreviations unit) abbrevs))
  (dolist (abbrev abbrevs) (setf (glactualunit abbrev) unit))
  unit)

; 04 Nov 92
; Define derived measurement units.  Args are unit type and a list of units.
; Unit type may be a named constant, such as mass, or nil.
; Each unit is specified by a list of: (unit-name unit-spec synonyms)
(defun gldefderivedunits (unit-type units)
  (dolist (unitlist units)
    (let ((dim (glunitdim (second unitlist))))
      (if unit-type (unless (= (gldimension unit-type) dim)
			    (error "Bad unit spec ~S" unitlist)))
      (setf (glexpansion (first unitlist)) (second unitlist))
      (gldefunit (first unitlist)
		 (or unit-type dim)
		 (glunitfactor (second unitlist))
		 (third unitlist)) )))

; 04 Nov 92; 04 May 93; 03 Jun 93; 28 Apr 95; 03 Apr 02; 18 Apr 03
; Convert from one unit to another.
; Returns a numeric factor or nil if conversion is improper.
; sets *glunitmethod* to w2m m2w m2e e2m if special conversion done.
(defun glconvertunit (from to)
  (let (f dim)
    (setq *glunkunits* nil)
    (setq *glunitdimerror* nil)
    (if (and (glunitp from) (glunitp to)) 
	(progn
	  (setq f (/ (glunitfactor from) (glunitfactor to)))
	  (setq dim (- (glunitdim from) (glunitdim to)))
	  (setq *glunitmethod* nil)
	  (if (= dim 0)
	      f
	    (if *glunitenable*
		(cond ((= dim *gldimweighttomass*)
		        (setq *glunitmethod* 'w2m) (/ f 9.80665))
		      ((= dim *gldimmasstoweight*)
		        (setq *glunitmethod* 'm2w) (* f 9.80665))
		      ((= dim *gldimmasstoenergy*)
		        (setq *glunitmethod* 'm2e) (* f 8.987554305625E16))
		      ((= dim *gldimenergytomass*)
		        (setq *glunitmethod* 'e2m) (/ f 8.987554305625E16))
		      (t (setq *glunitdimerror* t) nil))
	        (progn (setq *glunitdimerror* t) nil))))) ))

; 04 Nov 92; 30 Nov 92; 03 Apr 02
; Test whether unit is a legitimate unit specification
(defun glunitp (unit)
  (or (numberp unit)
      (and (glconstantp unit)
	   (numberp (glconstval unit)))
      (and unit
	   (symbolp unit)
	   (or (glsiconversion unit)
	       (glactualunit unit)))
      (and (consp unit)
	   (eq (car unit) '*)
	   (every #'glunitp (cdr unit)))
      (and (consp unit)
	   (eq (car unit) '/)
	   (= (length unit) 3)
	   (every #'glunitp (cdr unit)))
      (progn (if (atom unit) (push unit *glunkunits*))
	     nil) ) )

; 04 Nov 92; 03 Jun 93
; Find dimension from a unit expression.
(defun glunitdim (unit)
  (if (atom unit)
      (if (numberp unit)
	  0
	  (if (symbolp unit)
	      (or (gldimension unit)
		  (gldimension (glactualunit unit)))
	      (error "~A is not a unit")))
      (if (eq (car unit) '*)
	  (let ((dim 0))
	    (dolist (u (cdr unit) dim)
	      (setq dim (+ dim (glunitdim u)))))
	  (if (eq (car unit) '/)
	      (- (glunitdim (cadr unit))
		 (glunitdim (caddr unit)))
	      (error "~A has bad unit operator.~%" unit)))) )

; 04 Nov 92
; Find  conversion to SI units of a unit expression.
(defun glunitfactor (unit)
  (if (atom unit)
      (if (numberp unit)
	  unit
	  (if (symbolp unit)
	      (or (glsiconversion unit)
		  (glsiconversion (glactualunit unit)))))
      (if (eq (car unit) '*)
	  (let ((f 1.0))
	    (dolist (u (cdr unit) f)
	      (setq f (* f (glunitfactor u)))))
	  (if (eq (car unit) '/)
	      (/ (glunitfactor (second unit))
		 (glunitfactor (third unit)))
	      (error "~A has bad unit operator.~%" unit)))) )

; 03 Apr 02
; get actual unit name
(defun glactunit (u)
  (if (glsiconversion u)
      u
      (glactualunit u)))

; 16 July 93; 19 Jul 93; 05 Mar 99; 18 Apr 03
; Expand a unit into a flat quotient of simple factors
(defun glunitexpand (unit) (glunitexpandb unit (list nil nil) nil))
(defun glunitexpandb (unit flat flg)
  (if (atom unit)
      (if (numberp unit)
	  (if (= unit 1)
	      flat
	      (glunitpush unit flat flg))
	  (if (symbolp unit)
	      (progn
		(if (glactualunit unit) (setq unit (glactualunit unit)))
		(if (eq unit 'unity)
		    flat
		    (if (glexpansion unit)
			(glunitexpandb (glexpansion unit) flat flg)
			(if (and (gldimension unit) (= (gldimension unit) 0))
			    (glunitpush (glsiconversion unit) flat flg)
			    (glunitpush unit flat flg)))))
	      (error "~A has bad unit contents.~%" unit)))
      (if (eq (car unit) '*)
	  (dolist (x (cdr unit) flat)
	    (setq flat (glunitexpandb x flat flg)))
	  (if (eq (car unit) '/)
	      (glunitexpandb (second unit)
			    (glunitexpandb (third unit) flat (not flg))
			    flg)
	      (error "~A has bad unit operator.~%" unit)) ) ) )

; 19 Jul 93
; Expand a unit into a flat quotient of simple factors, removing duplicates
(defun glunitexpandc (unit)
  (let (flat num den)
    (setq flat (glunitexpand unit))
    (setq num (glunitsort (first flat)))
    (setq den (glunitsort (second flat)))
    (list (glmultiset-diff num den)
	  (glmultiset-diff den num)) ))

; 16 July 93
; Push a unit onto numerator, or denominator if flg = t.
(defun glunitpush (unit flat flg)
  (if flg (push unit (second flat))
          (push unit (first flat)))
  flat)

; 30 Nov 92
; Invert a unit speciication
(defun glinvertunit (u)
  (if (and (consp u)
	   (eq (car u) '/))
      (list '/ (third u) (second u))
      (list '/ 1 u)))

; 04 Aug 93
(defun glmultunits (ua ub) (list '* ua ub))
(defun gldivunits  (ua ub) (list '/ ua ub))

; 05 Mar 99; 09 Mar 99; 03 May 01; 18 Apr 03
; Simplify a unit expression, leaving it as is when possible
(defun glsimplunit (unit)
  (let (flat)
    (if (atom unit)
	unit
        (if (and (consp unit)
		 (eq (car unit) '*)
		 (or (eq (cadr unit) 'unity)
		     (eq (caddr unit) 'unity)))
	    (if (eq (cadr unit) 'unity)
		(caddr unit)
	        (cadr unit))
	    (if (and (consp unit)
		     (eq (car unit) '/)
		     (eq (caddr unit) 'unity))
		(cadr unit)
	        (progn (setq flat (glunitexpandc unit))
		       (glflattounit (car flat) (cadr flat) 1.0)) ) ) ) ))

; 16 Jul 93; 19 Jul 93; 03 Mar 95
; Simplify a unit expression.
(defun glsimplifyunit (unit &optional system)
  (let (flat (factor 1.0) num den tmp tmpb (progress t) max lng best inv)
    (or system (setq system (or (gldominantunit unit) 'si)))
    (setq flat (glunitexpand unit))
    (setq tmp (glsimpsystem (first flat) system))
    (setq tmpb (glsimpsystem (second flat) system))
    (setq num (glmultiset-diff (second tmp) (second tmpb)))
    (setq den (glmultiset-diff (second tmpb) (second tmp)))
    (setq factor (/ (first tmp) (first tmpb)))
    (while progress
      (setq progress nil)
      (setq max 0)
      ; (format t "num = ~A~%den = ~A~%" num den)
      (dolist (lst (glsystemunits system))
	(setq flat (fourth lst))
	(setq lng (+ (length (first flat)) (length (second flat))))
	(when (and (> lng 1)
		   (or (> lng max)
		       (and (= lng max) inv)))
	  (if (and (glsubmultiset (first flat) num)
		   (glsubmultiset (second flat) den))
	      (progn (setq max lng)
		     (setq inv nil)
		     (setq best lst))
	      (if (and (glsubmultiset (first flat) den)
		       (glsubmultiset (second flat) num))
		  (progn (setq max lng)
			 (setq inv t)
			 (setq best lst))))))
      (when (> max 0)
	(setq progress t)
	; (format t "best = ~A~%" best)
	(setq flat (fourth best))
	(if inv
	    (progn (setq num (glmultiset-diff num (second flat)))
		   (setq den (glunitsort
			       (cons (second best)
				     (glmultiset-diff den (first flat)))))
		   (setq factor (* factor (third best))))
	    (progn (setq num (glunitsort
			       (cons (second best)
				     (glmultiset-diff num (first flat)))))
		   (setq den (glmultiset-diff den (second flat)))
		   (setq factor (/ factor (third best)))))))
    ; (format t "num = ~A~%den = ~A~%" num den)
    (glflattounit num den factor) ))

; 19 Jul 93; 12 May 95
; Make a unit from two flat lists
(defun glflattounit (num den factor)
    (setq num (if (glunitapprox= factor 1.0)
		  (if (cdr num) (cons '* num) (car num))
		  (if num (cons '* (cons (or (glpowerten factor) factor) num))
		      (or (glpowerten factor) factor))))	
    (setq den (if (cdr den) (cons '* den) (car den)))
    (if (and num den)
	(list '/ num den)
	(or num
	    (if den (list '/ 1 den)
		'unity))) )

; 12 May 95
(defun glunitapprox= (x y) (< (abs (- x y)) 1.0e-6))

; Test if a factor is equivalent to a standard power of ten.
(defun glpowerten (x)
  (let (logx rlogx)
    (and (numberp x)
	 (plusp x)
	 (setq logx (log x 10))
	 (setq rlogx (round logx))
	 (glunitapprox= logx rlogx)
	 (cadr (assoc rlogx
		      '((24 yotta) (21 zetta) (18 exa) (15 peta) (12 tera)
			(9 giga) (6 mega) (3 kilo) (-3 milli) (-6 micro)
			(-9 nano) (-12 pico) (-15 femto) (-18 atto)
			(-21 zepto) (-24 yocto))))) ))

; 19 Jul 93
; Convert units list to a specified system
(defun glsimpsystem (units system)
  (let ((factor 1.0) lst f new)
    (dolist (x units)
      (if (numberp x)
	  (setq factor (* factor x))
	  (progn 
	    (if (and (setq new (glsystemunit x system))
		     (not (eq new x)))
		(if (setq f (glconvertunit x new))
		    (progn (setq factor (* factor f))
			   (push new lst))
		    (error "Failed to convert ~A to ~A~%" x new))
		(push x lst)))))
    (list factor (glunitsort lst))))

; 19 Jul 93
; Sort a list of symbols alphabetically
(defun glunitsort (lst) (sort lst #'glunitsortp))
		
; 19 Jul 93
(defun glunitsortp (x y)
  (or (numberp x)
      (and (symbolp x) (symbolp y)
	   (string< (symbol-name x) (symbol-name y)))))

; 19 Jul 93
; Test if first list is a sub-multiset of the second (both sorted)
(defun glsubmultiset (seta setb)
  (or (null seta)
      (and setb
	   (if (eq (car seta) (car setb))
	       (glsubmultiset (cdr seta) (cdr setb))
	       (if (glunitsortp (car setb) (car seta))
		   (glsubmultiset seta (cdr setb)))))))

; 19 Jul 93
; Multiset difference, seta - setb (both sorted)
(defun glmultiset-diff (seta setb)
  (if seta
      (if (null setb)
	  seta
	  (if (eq (car seta) (car setb))
	      (glmultiset-diff (cdr seta) (cdr setb))
	      (if (glunitsortp (car seta) (car setb))
		  (cons (car seta) (glmultiset-diff (cdr seta) setb))
		  (glmultiset-diff seta (cdr setb)))))))

; 07 Dec 92; 17 Dec 92; 23 Jul 93; 28 Apr 94
; Divide a units list "in half" for sqrt
(defun glsqrtunit (unit &optional system noerror)
  (let (flat (factor 1.0) num den tmp tmpb ptr uniterr)
    (or system (setq system (gldominantunit unit)))
    (setq flat (glunitexpand unit))
    (setq tmp (glsimpsystem (first flat) system))
    (setq tmpb (glsimpsystem (second flat) system))
    (setq num (glmultiset-diff (second tmp) (second tmpb)))
    (setq den (glmultiset-diff (second tmpb) (second tmp)))
    (setq factor (/ (first tmp) (first tmpb)))
    (setq ptr num)
    (while (and ptr (not uniterr))
      (if (eq (car ptr) (cadr ptr))
	  (progn (rplacd ptr (cddr ptr))
		 (setq ptr (cdr ptr)))
	  (setq uniterr t)))
    (setq ptr den)
    (while (and ptr (not uniterr))
      (if (eq (car ptr) (cadr ptr))
	  (progn (rplacd ptr (cddr ptr))
		 (setq ptr (cdr ptr)))
	  (setq uniterr t)))
    (if uniterr
	(unless noerror (glerror 'glsqrtunit "bad unit ~A" unit))
        (glflattounit num den (sqrt factor))) ))

; 29 Apr 94
; Divide a units list "in thirds" for cbrt
(defun glcbrtunit (unit &optional system noerror)
  (let (flat (factor 1.0) num den tmp tmpb ptr uniterr)
    (or system (setq system (gldominantunit unit)))
    (setq flat (glunitexpand unit))
    (setq tmp (glsimpsystem (first flat) system))
    (setq tmpb (glsimpsystem (second flat) system))
    (setq num (glmultiset-diff (second tmp) (second tmpb)))
    (setq den (glmultiset-diff (second tmpb) (second tmp)))
    (setq factor (/ (first tmp) (first tmpb)))
    (setq ptr num)
    (while (and ptr (not uniterr))
      (if (and (eq (car ptr) (cadr ptr))
	       (eq (car ptr) (caddr ptr)))
	  (progn (rplacd ptr (cdddr ptr))
		 (setq ptr (cddr ptr)))
	  (setq uniterr t)))
    (setq ptr den)
    (while (and ptr (not uniterr))
      (if (and (eq (car ptr) (cadr ptr))
	       (eq (car ptr) (caddr ptr)))
	  (progn (rplacd ptr (cdddr ptr))
		 (setq ptr (cddr ptr)))
	  (setq uniterr t)))
    (if uniterr
	(unless noerror (glerror 'glcbrtunit "bad unit ~A" unit))
        (glflattounit num den (cbrt factor))) ))

; 04 Nov 92; 03 Dec 92; 07 Dec 92; 17 Dec 92
; Simplify a unit expression.
;   (glsimplifyunit '(/ (* pound-force second second) (* slug foot))) ; = unity
; Still need to do: e.g.
;   (glsimplifyunit '(/ (* kilogram meter) (* second second)))  ; = newton
; 07 Dec 92
; Get numeric factor for a unit if it is a pure number
(defun glnumfactor (unit)
  (if (numberp unit)
      unit
      (if (glconstantp unit)
	  (glconstval unit)
	  (and (symbolp unit)
	       (= (gldimension unit)
		  (gldimension 'unity))
	       (glsiconversion unit)))) )

; 04 Nov 92
; Remove the first occurrence of an item from a list.
(defun remove-first (item lst)
  (if (consp lst)
      (if (eq item (first lst))
	  (rest lst)
	  (cons (first lst) (remove-first item (rest lst))))))

; 16 July 93; 29 Sep 94
; Find the dominant unit system used in a given unit
(defun gldominantunit (unit)
  (let (pairs system (max 0) val)
    (setq pairs (gldominantunitb unit pairs))
    (dolist (pair pairs)
      (setq val (+ (cdr pair)
		   (or (cdr (assoc (car pair)
				   '((si . 0.8) (cgs . 0.5) (english . 0.2))))
		       0)))
      (when (> val max)
	(setq max val)
	(setq system (car pair))))
    system ))

; 16 July 93; 29 Sep 94; 05 Mar 99; 17 Mar 99
(defun gldominantunitb (unit pairs)
  (let (unittype tmp aunit sys)
    (if (atom unit)
	(if (symbolp unit)
	    (if (setq sys
		      (or (and (setq unittype
				 (glunittype (setq aunit (glactualu unit))))
			       (caar (member aunit (glstdunits unittype)
					     :key #'cadr)))
			  (glunitsys unit)))
		(if (setq tmp (assoc sys pairs))
		    (progn (incf (cdr tmp)) pairs)
		    (push (cons sys 1) pairs))
	        pairs)
	    pairs)
	(progn (dolist (subunit (cdr unit))
		 (setq pairs (gldominantunitb subunit pairs)))
	       pairs)) ))

; 16 July 93
; find the system of units in which unit is used, if known.
(defun glunitsystem (unit)
  (let (unittype)
    (and (symbolp unit)
	 (setq unittype (glunittype unit))
	 (some #'(lambda (x) (if (eq unit (cadr x)) (car x)))
			 (glstdunits unittype))) ))

; 16 July 93
; find the unit to be substituted for unit in the specified system
(defun glsystemunit (unit system)
  (let (unittype tmp)
    (and (symbolp unit)
	 (setq unittype (glunittype unit))
	 (if (setq tmp (assoc system (glstdunits unittype)))
	     (cadr tmp))) ))

(dolist (pair '((force              (/ (* mass length) (* time time)))
		(area               (* length length))
		(volume             (* length length length))
		(power              (/ (* mass length length)
				       (* time time time)))
		(energy             (/ (* mass length length) (* time time)))
		(speed              (/ length time))
                (acceleration       (/ length (* time time)))
                (pressure           (/ force area))
                (density            (/ mass volume))
                (charge             (* current time))
		(electric-potential (/ power current))
		(capacitance        (/ charge electric-potential))
		(resistance         (/ electric-potential current))
		(conductance        (/ current electric-potential))
		(magnetic-field     (/ mass (* current time time)))
		(magnetic-flux      (* magnetic-field area))
		(inductance         (/ magnetic-flux current))
		(frequency          (/ 1 time))
		(dose               (/ (* length length) (* time time))) ) )
  (let ((dim (glunitdim (second pair))))
    (setf (gldimension (first pair)) dim)
    (pushnew (list dim (first pair)) *gldimtounittype*) ))

(gldefsimpleunits 'dimensionless
               '((radian    1.0                (radians))
		 (steradian 1.0                (sr steradians))
		 (degree    0.01745329251994   (deg degrees))
		 (arcminute 0.0002908882086657 (arcmin arcminutes arc-minute
						       arc-minutes))
		 (arcsecond 4.848136811095e-6  (arcsec arcseconds arc-second
						       arc-seconds))
		 (pi             3.1415926535897931 ())
		 (unity          1.0       ())
		 (dozen          12.0      ())
		 (gross          144.0     ())
		 (ten            10.0      ())
		 (hundred        100.0     ())
		 (thousand       1000.0    ())
		 (million        1.0e6     ())
		 (billion        1.0e9     ())
		 (trillion       1.0e12    ())
		 (quadrillion    1.0e15    ())
		 (quintillion    1.0e18    ())
		 (percent        0.01      (\% percent))
		 (tenth          0.1       ())
		 (hundredth      0.01      ())
		 (thousandth     0.001     ())
		 (millionth      1.0e-6    ())
		 (billionth      1.0e-9    ())
		 (trillionth     1.0e-12   ())
		 (quadrillionth  1.0e-15   ())
		 (quintillionth  1.0e-18   ())
		 (yotta          1.0e24    (yotta-))
		 (zetta          1.0e21    (zetta-))
		 (exa            1.0e18    (exa-))
		 (peta           1.0e15    (peta-))
		 (tera           1.0e12    (tera-))
		 (giga           1.0e9     (giga-))
		 (mega           1.0e6     (mega-))
		 (kilo           1000.0    (kilo-))
		 (hecto          100.0     (hecto-))
		 (deka           10.0      (deca deka- deca-))
		 (deci           0.1       (deci-))
		 (centi          0.01      (centi-))
		 (milli          0.001     (milli-))
		 (micro          1.0e-6    (micro-))
		 (nano           1.0e-9    (nano-))
		 (pico           1.0e-12   (pico-))
		 (femto          1.0e-15   (femto-))
		 (atto           1.0e-18   (atto-))
		 (zepto          1.0e-21   (zepto-))
		 (yocto          1.0e-24   (yocto-))
		 ))

(gldefsimpleunits 'length
               '((meter         1.0       (m meters metre))
		 (foot          0.3048    (ft feet))
		 (decimeter     0.1       (dm decimeters decimetre))
		 (centimeter    0.01      (cm centimeters centimetre))
		 (millimeter    0.001     (mm millimeters millimetre))
		 (dekameter     10.0      (dam dekameters decameter
					       decameters decametre))
		 (hectometer    100.0     (hm hectometers hectometre))
		 (kilometer     1000.0    (km kilometers kilometre))
		 (micron        1.0e-6    (um micro-meter micrometer
					      micrometers micro-meters
					      microns micrometre))
		 (nanometer     1.0e-9    (nm nanometers nanometre))
		 (angstrom      1.0e-10   (a  angstroms))
		 (inch          0.0254    (in inches))
		 (mile          1609.344  (mi miles))
		 (nautical-mile 1852.0    (nm nauticalmiles
					      nauticalmile nautical-miles))
                 (astronomical-unit 
                                1.49598e11 (au))
		 (light-year    9.46e15    (ly light-years
					       lightyear lightyears))
		 (parsec        3.083e16   (parsecs))
		 (fathom        1.8054     (fathoms))
		 (yard          0.9144     (yd yards))
		 (rod           5.0292     (rods))
		 (mil           0.0000254  (mils))
		 (furlong       201.168    (furlongs)) ) )

(dolist (x '(foot inch mile nautical-mile fathom yard rod furlong
	     pound slug pound-force ounce-force pound ounce ton long-ton
	     hundredweight dram grain pennyweight scruple acre square-mile
	     cubic-inch cubic-foot cubic-yard cubic-mile acre-foot gallon
	     quart peck bushel fifth pint cup fluid-ounce gill fluidram
	     minim tablespoon teaspoon foot-pound horsepower-hour grain
	     horsepower british-thermal-unit  btu pounds-per-square-inch psi
	     miles-per-hour miles-per-second feet-per-second knot
	     square-foot square-yard square-inch))
  (setf (glunitsys x) 'english))

(gldefsimpleunits 'mass
               '((kilogram         1.0           (kg kilograms))
		 (hectogram        0.1           (hg hectograms))
		 (dekagram         0.01     (dag dekagrams decagram decagrams))
		 (gram             0.001         (gm grams))
		 (decigram         0.0001        (dg decigrams))
		 (centigram        0.00001       (cg centigrams))
		 (milligram        1.0e-6        (mg milligrams))
		 (microgram        1.0e-9        (ug micrograms))
		 (metric-ton       1000.0        (metric-tons tonne tonnes))
		 (pound            0.45359237    (lb lbs pounds))    ; exactly
		 (slug             14.593902937  (slugs))
                    ; derived 02 Jun 95 based on pound, foot, and earth-gravity
		 (atomic-mass-unit 1.6605402e-27 (amu atomic-mass-units))
		 (earth-mass       5.98e24       ()) ) )

(gldefsimpleunits 'time
               '((second      1.0        (s sec secs seconds)) ))

(gldefderivedunits 'time
		   '((millisecond (* milli second)     (ms msec millisec
							   milliseconds))
		     (microsecond (* micro second)     (us usec microsec
							   microseconds))
		     (nanosecond  (* nano  second)     (ns nsec nanosec
							   nanoseconds))
		     (picosecond  (* pico  second)     (ps psec picosec
							   picoseconds))
		     (femtosecond (* femto second)     (femtoseconds femtosec))
		     (attosecond  (* atto  second)     (attoseconds attosec))
		     (minute      (* 60    second)     (min minutes))
		     (hour        (* 3600  second)     (hr hours))
		     (day         (* 86400 second)     (days))
		     (week        (* 604800 second)    (wk weeks))
		     (fortnight   (* 1209600 second)   (fortnights))
		     (month       (* 2629800 second)   (mon months))
		     (year        (* 31557600 second)  (yr years))
		     (century     (* 3.15576e9 second) (centuries)) ) )

(gldefsimpleunits 'frequency '( (hertz     1.0 (hz))
			       (becquerel 1.0 (bq)) ) )

(gldefderivedunits 'frequency
		   '((kilohertz   (* kilo hertz)       (khz))
		     (megahertz   (* mega hertz)       (mhz))
		     (gigahertz   (* giga hertz)       (ghz))
		     (terahertz   (* tera hertz)       (thz))
		     (curie       (* 3.7e10 becquerel) (curies)) ) )

(gldefsimpleunits 'current
               '((ampere      1.0       (amp amps amperes)) ))

(gldefderivedunits 'acceleration
                 '((earth-gravity (* 9.80665 (/ meter (* second second))))
		   (feet-per-second-squared (/ foot (* second second))
		     (foot-per-second-squared ft/s/s ft/sec/sec foot/sec/sec
		      ft/s2 ft/sec2 foot/second/second))
		   (meters-per-second-squared (/ meter (* second second))
		      (meter-per-second m/s/s m/sec/sec m/second/second
			m/s2 m/sec2 meter/sec/sec meter/second/second))
		   (centimeters-per-second-squared
		     (/ centimeter (* second second))
		     (centimeter-per-second-squared cm/s/s cm/sec/sec cm/s2
						      cm/sec2))
		   ))

(gldefderivedunits 'current
		   '((milliampere (* milli ampere)
				  (milliamp milliamps ma milliampere))
		     (microampere (* micro ampere)
				  (microamp microamps ua microamperes))
		     (abampere    (* 10 ampere) (abamp abamperes))
		     (statampere  (* 3.336e-10 ampere) (statamp statamperes))
		     ))

(gldefderivedunits 'electric-potential
		   '((volt      (/ (* kilogram meter meter)
				   (* ampere second second second))
				                (v volts))
		     (millivolt (* milli volt)  (mv millivolts))
		     (microvolt (* micro volt)  (uv microvolts))
		     (abvolt    (* 1.0e-8 volt) (abvolts))
		     (statvolt  (* 299.8 volt)  (statvolts)) ))

(gldefderivedunits 'resistance
		   '((ohm      (/ (* kilogram meter meter)
				  (* ampere ampere second second second))
		                 (ohms))
		     (kilohm   (* kilo ohm)     (kilohms))
		     (megohm   (* mega ohm)     (megohms))
		     (abohm    (* nano ohm)     (abohms))
		     (statohm  (* 8.987e11 ohm) (statohms)) ))

(gldefderivedunits 'conductance
		   '((siemens      (/ (* ampere ampere second second second)
				      (* kilogram meter meter)) (mho) ) ))

(gldefderivedunits 'capacitance
		   '((farad   (/ (* ampere ampere second second second second)
				 (* kilogram meter meter))
			                             (farads))
		     (microfarad (* micro farad)     (uf microfarads))
		     (picofarad  (* pico farad)      (pf picofarads))
		     (abfarad    (* giga farad)      (abfarads))
		     (statfarad  (* 1.113e-12 farad) (statfarads)) ))

(gldefderivedunits 'inductance
		   '((henry      (/ (* kilogram meter meter)
				    (* ampere ampere second second))
				                    (henrys))
		     (millihenry (* milli henry)    (mh millihenrys))
		     (microhenry (* micro henry)    (uh microhenrys))
		     (abhenry    (* nano henry)     (abhenrys))
		     (stathenry  (* 8.987e11 henry) (stathenrys)) ))

(gldefderivedunits 'magnetic-flux
		   '((weber      (/ (* kilogram meter meter)
				    (* ampere second second))
				                   (wb webers))
		     (maxwell    (* 1.0e-8 weber)  (maxwells)) ))

(gldefderivedunits 'magnetic-field
		   '((tesla      (/ kilogram (* ampere second second))
				                  (teslas))
		     (gauss      (* 1.0e-4 tesla) ())
		     (milligauss (* milli gauss)  ()) ))

(gldefsimpleunits 'temperature
		 '((degree-kelvin      1.0       (k kelvin kelvins))
		   (degree-rankine     5/9       (rankine)) ))

(gldefsimpleunits 'luminosity
		 '((candela            1.0       (cd candelas)) ))

(gldefsimpleunits 'substance
		 '((mole               1.0       (mol moles)) ))

(gldefsimpleunits 'money
		 '((dollar             1.0       (dollars $)) ))

(gldefderivedunits 'force
                 '((pound-force  (/ (* slug foot) (* second second)) (lbf))
		   (ounce-force  (/ pound-force 16)        ())
		   (newton (/ (* kilogram meter) (* second second))
			    (nt newtons))
		   (dyne   (/ (* gram centimeter) (* second second))
			    (dynes))
		   (kilogram-force  (* kilogram earth-gravity)
				    (kgf kilogram-weight))
		   (gram-weight     (* gram earth-gravity) (gram-force)) ))

(gldefderivedunits 'mass
		 '((ounce  (/ pound 16)
			    (oz ounces))
		   (ton    (* 2000 pound)
			    (tons short-ton short-tons))
		   (long-ton (* 2240 pound)
			    (tons long-ton long-tons))
		   (hundredweight (* 100 pound) (hundredweights))
		   (dram   (/ ounce 16) (drams))
		   (grain  (/ dram 27.344) (grains))
		   (troy-pound (* 0.373 kilogram) (troy-pounds))
		   (troy-ounce (* 31.103 gram)
			       (troy-ounces ounce-troy ounces-troy))
		   (pennyweight (* 1.555 gram) (pennyweights))
		   (scruple (* 1.296 gram) (scruples))
		   ))

(gldefderivedunits 'area
                 '((square-meter (* meter meter)
				 (m^2 meter-squared meters-squared 
				      metersquared square-meters))
		   (square-centimeter (* centimeter centimeter)
				      (cm^2 centimetersquared
					    centimeters-squared
					    centimeter-squared
					    square-centimeters))
		   (square-foot (* foot foot)
				(ft^2 foot-squared feet-squared footsquared
				      feetsquared square-feet))
		   (square-yard (* yard yard)
				(yard^2 yard-squared yardsquared yards-squared
					square-yards))
		   (square-inch (* inch inch)
				(in^2 inch-squared inchsquared inches-squared
				      square-inches))
		   (hectare (* 10000 metersquared)
			    (hectares))
		   (are     (* 100 metersquared) (ares))
		   (acre (* 43560 footsquared)
			 (acres))
		   (square-mile (* mile mile)
				(mile^2 mile-squared miles-squared milesquared
					square-miles))
		   (square-kilometer (* kilometer kilometer)
				     (km^2 kilometer-squared
					   kilometers-squared
					   kilometersquared
					   square-kilometers))
		   (square-millimeter (* millimeter millimeter)
				      (mm^2 millimeter-squared
					    millimeters-squared
					    millimetersquared
					    square-millimeters))
		   (square-micron (* micrometer micrometer)
				      (um^2 micrometer-squared
					    micrometers-squared
					    micron-squared microns-squared
					    micrometersquared
					    micronsquared square-microns))
		   (barn (* 1.0e-28 metersquared) (barns))
		   ))

(gldefderivedunits 'volume
		 '((cubic-meter     (* meter meter meter)
			            (m^3 meter-cubed metercubed meters-cubed
					 cubic-meters kiloliter kiloliters
					 kilolitre))
		   (cubic-centimeter (* centimeter centimeter centimeter)
				    (cm^3 centimeter-cubed centimeters-cubed
					  centimetercubed centimeterscubed
					  cubic-centimeters milliliter
					  milliliters ml cc
					  cubic-centimetre millilitre))
		   (cubic-millimeter (* millimeter millimeter millimeter)
				    (mm^3 millimeter-cubed millimeters-cubed
					  millimetercubed millimeterscubed
					  cubic-millimeters cubic-millimetre))
		   (cubic-micron     (* micron micron micron)
				     (micron-cubed microns-cubed
						   cubic-microns))
		   (cubic-kilometer  (* kilometer kilometer kilometer)
				    (km^3 kilometer-cubed kilometers-cubed
					  kilometercubed kilometerscubed
					  cubic-kilometers cubic-kilometre))
		   (cubic-inch       (* inch inch inch)
			            (in^3 inch-cubed inchcubed inchescubed
					  cubic-inches))
		   (cubic-foot       (* foot foot foot)
			            (ft^3 foot-cubed footcubed feetcubed
					  cubic-feet))
		   (cubic-yard       (* yard yard yard)
			            (yd^3 yard-cubed yardcubed yardscubed
					  yards-cubed cubic-yards))
		   (cubic-mile      (* mile mile mile)
			            (mile^3 mile-cubed miles-cubed
					     cubic-miles))
		   (acre-foot        (* acre foot)
			            (acrefoot acre-feet acrefeet))
		   (liter           (* 0.001 metercubed)
			            (l liters litre cubic-decimeter
				       cubic-decimeters))
		   (deciliter       (/ liter 10)
			            (dl deciliters decilitre))
		   (centiliter      (/ liter 100) (cl centiliters centilitre))
		   (dekaliter       (* liter 10)
				    (dekaliters decaliter decaliters decalitre
						dekalitre))
		   (hectoliter      (* 100 liter) (hectoliters hectolitre))
		   (gallon          (* 3.785411784 liter) (gal gallons))
		   (quart           (/ gallon 4) (qt quarts))
		   (peck            (* 8 quart) (pecks))
		   (bushel          (* 4 peck) (bushels))
		   (fifth           (/ gallon 5) (fifths))
		   (pint            (* 0.473 liter) (pt pints))
		   (cup             (/ pint 2) (cups))
		   (fluid-ounce     (* 0.029573 liter)
			            (floz fluidounce fluidounces fluid-ounces))
		   (gill            (* 4 fluid-ounce) (gills))
		   (fluidram        (* 3.5516 cubic-centimeter) (fluidrams))
		   (minim           (* 0.059194 cubic-centimeter) (minims))
		   (tablespoon      (/ fluidounce 2) (tbsp tablespoons))
		   (teaspoon        (/ tablespoon 3) (tsp teaspoons)) ) )

(gldefderivedunits 'power
                 '((watt       (/ (* kilogram meter meter)
				  (* second second second))
			       (w watts))
		   (milliwatt  (* milli watt)
			       (mw milli-watt milli-watts))
		   (microwatt  (* micro watt)
			       (uw micro-watt micro-watts))
		   (kilowatt   (* kilo watt)
			       (kw kilowatts))
		   (megawatt   (* mega watt)
			       (mw megawatts mega-watt mega-watts))
		   (gigawatt   (* giga watt)
			       (gw gigawatts giga-watt giga-watts))
		   (horsepower (* 550 (/ (* foot pound-force) second))
			       (hp)) ) )

(gldefderivedunits 'energy
		 '((joule (/ (* kilogram meter meter) (* second second))
			  (j joules))
		   (foot-pound (* foot pound-force)
			      (ftlb ft-lb footpound footpounds foot-pounds))
		   (kilowatt-hour (* kilo watt hour)
				 (kwh kilowatthour kilowatthours
				      kilowatt-hours))
		   (watt-hour (* watt hour)
			     (watthour watthours watt-hours))
		   (horsepower-hour (* horsepower hour)
				    (hp-hour))
		   (electron-volt (* 1.60217733e-19 joule)
				  (ev electronvolt electronvolts
				      electron-volts))
		   (mev (* 1.60217733e-13 joule)
			(mega-electron-volts))
		   (gev (* 1.60217733e-10 joule)
			(giga-electron-volts))
		   (tev (* 1.60217733e-7 joule)
			(tera-electron-volts))
		   (calorie (* 4.184 joule)
			    (cal calorie calories))
		   (kilocalorie (* 4184.0 joule)
				(kcal kilo-calorie kilo-calories))
		   (british-thermal-unit (* 1055.056 joule)
				       (btu btus britishthermalunit
					    britishthermalunits
					    british-thermal-units))
		   (erg (* 1.0e-7 joule)
			(ergs)) ) )

(gldefderivedunits 'charge
  '((coulomb     (* ampere second)     (coul coulombs))
    (abcoulomb   (* 10.0 coulomb)      (abcoul abcoulombs))
    (statcoulomb (* 3.336e-10 coulomb) (statcoul statcoulombs))
    (amperehour  (* 3600.0  coulomb)   (amp-hour ampere-hour
				        amperehours ampere-hours)) ))

(gldefderivedunits 'pressure
  '((pounds-per-square-inch (/ (* 144 pound-force) (* foot foot)) (psi))
    (pascal     (/ newton (* meter meter)) (pa))
    (kilopascal (* 1000.0 pascal) (kilo-pascal kpa kilopascals))
    (bar        (* 1.0e5 pascal)  (bars))
    (millibar   (* milli bar)     (millibars))
    (torr       (* (/ 101325 760) pascal) ())
    (dynes-per-square-centimeter (/ dyne (* centimeter centimeter)))
    (atmosphere (* 101325 pascal) (atm)) ))

(gldefderivedunits 'speed
                 '((miles-per-hour (/ mile hour) (mph mile-per-hour))
		   (miles-per-second (/ mile second) (mile-per-second))
		   (kilometers-per-hour (/ kilometer hour)
					(kph kilometer-per-hour))
		   (kilometers-per-second (/ kilometer second)
					  (kps kilometer-per-second))
		   (feet-per-second (/ foot second)
				    (foot-per-second fps ft/s ft/sec foot/sec
						     ft/second foot/second))
		   (meters-per-second (/ meter second)
				      (meter-per-second m/s m/sec m/second
				       meter/sec meter/second))
		   (centimeters-per-second (/ centimeter second)
					   (centimeter-per-second cm/s cm/sec))
		   (knot              (/ nautical-mile hour) (knots))
		   (speed-of-light    (* 299792458 (/ meter second)))
		   ))

(gldefderivedunits 'dose      ; of radiation
		   '((gray    (/ joule kilogram)   (gy))
		     (sievert (/ joule kilogram)   (sv))
		     (rad     (/ gray 100)         ())
		     (rem     (/ sievert 100)      ()) ))


(dolist (x '(si cgs english)) (setf (glsystemunits x) nil))

(dolist (pair '((length   (si meter)        (cgs centimeter) (english foot))
		(mass     (si kilogram)     (cgs gram)       (english slug))
		(time     (si second)       (cgs second)     (english second))
		(force    (si newton)       (cgs dyne)    (english pound-force))
		(area     (si square-meter) (cgs square-centimeter)
		          (english square-foot))
		(volume   (si cubic-meter)  (cgs cubic-centimeter)
		          (english cubic-foot))
		(power    (si watt)         (cgs watt)  (english horsepower))
		(energy   (si joule)        (cgs erg)   (english foot-pound))
		(pressure (si pascal)       (cgs dynes-per-square-centimeter)
			  (english pounds-per-square-inch))
		(speed               (si meters-per-second)
				     (cgs centimeters-per-second)
				     (english feet-per-second))
                (acceleration        (si )
				     (cgs ) (english ))
                (density             (si ) (cgs ) (english ))
                (charge              (si coulomb) (cgs ) (english ))
		(electric-potential  (si volt) (cgs ) (english ))
		(capacitance         (si farad) (cgs ) (english ))
		(resistance          (si ohm) (cgs ) (english ))
		(conductance         (si siemens) (cgs ) (english ))
		(magnetic-field      (si tesla) (cgs ) (english ))
		(magnetic-flux       (si weber) (cgs ) (english ))
		(inductance          (si henry) (cgs ) (english ))

		))
  (setf (glstdunits (car pair)) (cdr pair))
  (dolist (pr (cdr pair))
    (let (tmp)
      (when (cdr pr)
	(setq tmp (glunitexpandc (cadr pr)))
	(push (list (car pair) (cadr pr)
		    (if (numberp (caar tmp)) (caar tmp) 1.0)
		    (if (numberp (caar tmp)) (cons (cdar tmp) (cdr tmp)) tmp))
	      (glsystemunits (car pr)))) ) ))

(defconstant *speed-of-light*    '(q 2.99792458e8 (/ meter second)))
(defconstant *gravitational-constant*
             '(q 6.6720e-11  (/ (* meter meter meter)
			     (* kilogram second second))))
(defconstant *elementary-charge* '(q 1.6021892e-19 coulomb))
(defconstant *electron-mass*     '(q 9.109534e-31 kilogram))
(defconstant *earth-gravity*     '(q 9.80665 (/ meter (* second second))))
