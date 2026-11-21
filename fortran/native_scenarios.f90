! SPDX-License-Identifier: GPL-3.0-only
! All scenario configuration and mechanisms execute natively. Generated constants are build-time data.
module native_scenarios
 use iso_c_binding
 use r2_math
 use, intrinsic::ieee_arithmetic
 implicit none
 integer,parameter::scenario_count=51
 integer,save::p_scenario=0
 character(len=40),parameter::scenario_ids(51)=[character(len=40):: &
  'hw-edge', &
  'hw-peltier', &
  'hw-lid', &
  'hw-optics-misaligned', &
  'hw-crosstalk', &
  'hw-led-aged', &
  'hw-led-drift', &
  'hw-saturation', &
  'hw-noise', &
  'hw-dirty', &
  'hw-not-seated', &
  'hw-sensor-drift', &
  'hw-fan', &
  'hw-lid-opened', &
  'sw-baseline', &
  'sw-threshold', &
  'sw-colorcomp', &
  'sw-passive-ref', &
  'sw-dropout', &
  'sw-template', &
  'sw-melt', &
  'sw-crash', &
  're-polymerase', &
  're-bleach', &
  're-probe-degraded', &
  're-dntp', &
  're-inhibitor', &
  're-plastic-profile', &
  're-seal', &
  're-autofluor', &
  're-lot', &
  're-hotstart', &
  're-mismatch', &
  'hu-anneal-high', &
  'hu-anneal-low', &
  'hu-dye', &
  'hu-rotated', &
  'hu-no-controls', &
  'hu-seal', &
  'hu-dna-amount', &
  'hu-bubbles', &
  'hu-pipetting', &
  'hu-aerosol', &
  'hu-mixing', &
  'hu-droplets', &
  'hu-no-spin', &
  'in-nonsigmoid', &
  'in-efficiency', &
  'in-highcq', &
  'in-blank', &
  'in-refgene']
 character(len=160),parameter::scenario_titles(51)=[character(len=160):: &
  'Thermal block non-uniformity (edge effect)', &
  'Peltier element degradation (zone 1 pumps 70 % of nominal)', &
  'Heated lid malfunction (lid at 60  C, condensation)', &
  'Optical misalignment (vignetting across the plate)', &
  'Optical crosstalk (filter shifted: 30 % FAM into the YY channel)', &
  'Light source degradation (LED at 50 %)', &
  'Light source drifting within the run (end of life)', &
  'Detector saturation (gain set too high)', &
  'Detector noise (camera read scatter x8)', &
  'Dirty optical path (spill over a 4 x 4 patch)', &
  'Plate not seated flush (drawer jam; poor thermal contact in rows A-D)', &
  'Block temperature sensor drifting low (added)', &
  'Cooling fan stalled (added)', &
  'Lid opened during the run (added)', &
  'Baseline misassignment (very early amplification)', &
  'Inappropriate threshold placement', &
  'Missing colour compensation (crosstalk not corrected)', &
  'Passive-reference normalisation applied to a mix without ROX', &
  'Firmware-software communication dropout', &
  'Corrupted / outdated run template', &
  'Melt-curve derivative errors', &
  'Software crash before saving (long run)', &
  'Polymerase inactivation (master mix 6 months at room temperature)', &
  'Fluorophore photobleaching (probe 24 h in lab light)', &
  'Probe degradation (quencher separated: high baseline)', &
  'dNTP hydrolysis (nucleotides run out in the late cycles)', &
  'Inhibitors in the template (ethanol / humic acid carry-over)', &
  'Incompatible plastics (wrong plate profile)', &
  'Non-optical sealing film', &
  'Autofluorescent plastics', &
  'Lot-to-lot variability (new primer/probe lot at 60 % / 70 %)', &
  'No hot start, plate left 4 h on a warm bench (added)', &
  'Probe mismatch to the target variant (added)', &
  'Wrong thermal profile: annealing too high (68  C)', &
  'Wrong thermal profile: annealing too low (50  C)', &
  'Incorrect fluorophore selection (P-35S read on the wrong channel)', &
  'Plate loaded rotated 180 ', &
  'Missing controls (NTC and extraction blank omitted)', &
  'Inadequate sealing (edge-well evaporation)', &
  'Wrong DNA amount (template 16 x too low) (added)', &
  'Air bubbles in wells', &
  'Pipetting volume inaccuracy (CV 30 %)', &
  'Aerosol cross-contamination (amplicon carry-over, UNG off)', &
  'Incomplete master-mix homogenisation (gradient across columns)', &
  'Droplets on well walls (template enters 4 cycles late)', &
  'Plate not centrifuged', &
  'Accepting non-sigmoidal curves (baseline creep in negatives)', &
  'Ignoring amplification efficiency (event assay at 88 % of the reference efficiency)', &
  'Misinterpreting high Cq values (sample at 1/40 of the GM content)', &
  'Overlooking extraction contamination (in place of gDNA in -RT controls)', &
  'Unstable reference gene (reference varies +/-1.6 cycles by extract)']
 character(len=20),parameter::scenario_kinds(51)=[character(len=20):: &
  'native', &
  'native', &
  'native+injected', &
  'native', &
  'injected', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'analysis', &
  'injected', &
  'analysis', &
  'injected', &
  'native', &
  'not reproducible', &
  'injected', &
  'native', &
  'native', &
  'native', &
  'injected', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'native', &
  'injected', &
  'injected', &
  'native', &
  'injected', &
  'native', &
  'injected', &
  'native', &
  'native', &
  'native', &
  'injected', &
  'injected', &
  'injected', &
  'native', &
  'native', &
  'native', &
  'native']
 integer,parameter::scenario_categories(51)=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,5,5,5,5,5,6,6,6,6,6]
 integer,parameter::default_scopes(51)=[4,1,1,1,1,1,1,1,1,5,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,2,1,1,1,1,6,4,2,2,2,6,2,2,2,6,2,2,6,2]
 character(len=52),parameter::parameter_labels_flat(306)=[character(len=52):: &
  'Edge temperature deficit (C)', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Zone-1 pump fraction', '', &
  '', '', &
  '', 'Scenario seed offset', &
  'Lid target (C)', 'Condensation loss / cycle', &
  'Condensation starts at cycle', '', &
  '', 'Scenario seed offset', &
  'Far-column light transmission', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Extra FAM to YY leakage', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'LED output fraction', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'End-of-run LED fraction', 'Initial LED fraction', &
  'Drift starts at cycle', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Detector gain', 'ADC clipping ceiling (counts)', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Read noise multiplier', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Patch light transmission', 'Patch noise multiplier', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Contact lag time constant (s)', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Primary sensor drift (C / s)', 'Maximum sensor bias (C)', &
  'Drift starts (instrument s)', '', &
  '', 'Scenario seed offset', &
  'Fan stall starts (instrument s)', 'Stall detection delay (s)', &
  '', '', &
  '', 'Scenario seed offset', &
  'Lid opens (instrument s)', '', &
  '', '', &
  '', 'Scenario seed offset', &
  'Sample DNA multiplier', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Analysis net threshold (counts)', '', &
  '', '', &
  '', 'Scenario seed offset', &
  'Uncorrected FAM to YY leakage', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Incorrect ratio scale factor', '', &
  '', '', &
  '', 'Scenario seed offset', &
  'First missing cycle', 'Number of missing cycles', &
  '', '', &
  '', 'Scenario seed offset', &
  'Outdated annealing target (C)', 'Outdated cycle count', &
  '', '', &
  '', 'Scenario seed offset', &
  'Requested melt peak (C)', 'Derivative window points', &
  '', '', &
  '', 'Scenario seed offset', &
  'Last saved cycle before crash', '', &
  '', '', &
  '', 'Scenario seed offset', &
  'Remaining enzyme fraction', 'Remaining efficiency fraction', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Fluorescent probe fraction', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Unquenched reporter (nM)', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Retained plateau fraction', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Sample enzyme fraction', 'Sample efficiency fraction', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Plate contact lag (s)', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Seal optical transmission', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Added background counts', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Remaining primer fraction', 'Remaining probe fraction', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Warm-bench time (hours)', 'Warm bench temperature (C)', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Probe melting-point loss (C)', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Annealing target (C)', '', &
  '', '', &
  '', 'Scenario seed offset', &
  'Annealing target (C)', '', &
  '', '', &
  '', 'Scenario seed offset', &
  'Wrong-channel transfer fraction', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Rotation (degrees)', '', &
  '', '', &
  '', 'Scenario seed offset', &
  'Omit NTC (1=yes)', 'Omit extraction blank (1=yes)', &
  '', '', &
  '', 'Scenario seed offset', &
  'Late edge loss / cycle', 'First affected cycle', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Sample DNA divisor', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Number of bubbles / affected well', 'Signal loss at bubble', &
  'First bubble cycle', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Pipetting CV (percent)', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Contamination copies', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Far-column concentration fraction', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Template entry delay (cycles)', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Baseline offset (counts)', 'Template entry delay (cycles)', &
  'Early-cycle spike counts', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Negative-control creep / cycle', 'Creep starts at cycle', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Target / reference efficiency ratio', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Target DNA divisor', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Copies in extraction blank', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset', &
  'Reference variation (+/- cycles)', '', &
  '', 'Affected-well scope code', &
  'Well mask (0=scope, bits 1-14)', 'Scenario seed offset']
 character(len=52),parameter::parameter_names(6,51)=reshape(parameter_labels_flat,[6,51])
 real(c_double),parameter::scenario_defaults(6,51)=reshape([ &
  7d0,0d0,0d0,0d0,0d0,0d0, &
  0.7d0,0d0,0d0,0d0,0d0,0d0, &
  60d0,0.006d0,20d0,0d0,0d0,0d0, &
  0.35d0,0d0,0d0,0d0,0d0,0d0, &
  0.3d0,0d0,0d0,0d0,0d0,0d0, &
  0.5d0,0d0,0d0,0d0,0d0,0d0, &
  0.45d0,1d0,1d0,0d0,0d0,0d0, &
  5d0,4095d0,0d0,0d0,0d0,0d0, &
  8d0,0d0,0d0,0d0,0d0,0d0, &
  0.5d0,4d0,0d0,0d0,0d0,0d0, &
  20d0,0d0,0d0,0d0,0d0,0d0, &
  0.5d0,12d0,1200d0,0d0,0d0,0d0, &
  1800d0,3d0,0d0,0d0,0d0,0d0, &
  1500d0,0d0,0d0,0d0,0d0,0d0, &
  200d0,0d0,0d0,0d0,0d0,0d0, &
  1000d0,0d0,0d0,0d0,0d0,0d0, &
  0.3d0,0d0,0d0,0d0,0d0,0d0, &
  100d0,0d0,0d0,0d0,0d0,0d0, &
  18d0,3d0,0d0,0d0,0d0,0d0, &
  55d0,40d0,0d0,0d0,0d0,0d0, &
  82d0,5d0,0d0,0d0,0d0,0d0, &
  30d0,0d0,0d0,0d0,0d0,0d0, &
  0.25d0,0.65d0,0d0,0d0,0d0,0d0, &
  0.25d0,0d0,0d0,0d0,0d0,0d0, &
  25d0,0d0,0d0,0d0,0d0,0d0, &
  0.45d0,0d0,0d0,0d0,0d0,0d0, &
  0.5d0,0.65d0,0d0,0d0,0d0,0d0, &
  12d0,0d0,0d0,0d0,0d0,0d0, &
  0.12d0,0d0,0d0,0d0,0d0,0d0, &
  400d0,0d0,0d0,0d0,0d0,0d0, &
  0.6d0,0.7d0,0d0,0d0,0d0,0d0, &
  4d0,30d0,0d0,0d0,0d0,0d0, &
  8d0,0d0,0d0,0d0,0d0,0d0, &
  68d0,0d0,0d0,0d0,0d0,0d0, &
  50d0,0d0,0d0,0d0,0d0,0d0, &
  1d0,0d0,0d0,0d0,0d0,0d0, &
  180d0,0d0,0d0,0d0,0d0,0d0, &
  1d0,1d0,0d0,0d0,0d0,0d0, &
  0.025d0,25d0,0d0,0d0,0d0,0d0, &
  16d0,0d0,0d0,0d0,0d0,0d0, &
  1d0,0.65d0,12d0,0d0,0d0,0d0, &
  30d0,0d0,0d0,0d0,0d0,0d0, &
  60d0,0d0,0d0,0d0,0d0,0d0, &
  0.35d0,0d0,0d0,0d0,0d0,0d0, &
  4d0,0d0,0d0,0d0,0d0,0d0, &
  80d0,2d0,200d0,0d0,0d0,0d0, &
  10d0,15d0,0d0,0d0,0d0,0d0, &
  0.88d0,0d0,0d0,0d0,0d0,0d0, &
  40d0,0d0,0d0,0d0,0d0,0d0, &
  60d0,0d0,0d0,0d0,0d0,0d0, &
  1.6d0,0d0,0d0,0d0,0d0,0d0],[6,51])
 real(c_double),parameter::scenario_min(6,51)=reshape([ &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.1d0,0d0,0d0,0d0,0d0,-100000d0, &
  25d0,0d0,1d0,0d0,0d0,-100000d0, &
  0.05d0,0d0,0d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.01d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.01d0,0.1d0,1d0,0d0,0d0,-100000d0, &
  1d0,500d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.01d0,1d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.01d0,1d0,0d0,0d0,0d0,-100000d0, &
  0d0,0.1d0,0d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  2d0,1d0,0d0,0d0,0d0,-100000d0, &
  45d0,10d0,0d0,0d0,0d0,-100000d0, &
  60d0,3d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.001d0,0.05d0,0d0,0d0,0d0,-100000d0, &
  0.01d0,0d0,0d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.05d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.01d0,0.05d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.01d0,0d0,0d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.05d0,0.05d0,0d0,0d0,0d0,-100000d0, &
  0d0,20d0,0d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  60d0,0d0,0d0,0d0,0d0,-100000d0, &
  40d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.1d0,0d0,0d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  0d0,1d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,5d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  0.05d0,0d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0, &
  0d0,1d0,0d0,0d0,0d0,-100000d0, &
  0.3d0,0d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  1d0,0d0,0d0,0d0,0d0,-100000d0, &
  0d0,0d0,0d0,0d0,0d0,-100000d0],[6,51])
 real(c_double),parameter::scenario_max(6,51)=reshape([ &
  15d0,0d0,0d0,6d0,16383d0,100000d0, &
  1d0,0d0,0d0,0d0,0d0,100000d0, &
  110d0,0.05d0,44d0,0d0,0d0,100000d0, &
  1d0,0d0,0d0,6d0,16383d0,100000d0, &
  1d0,0d0,0d0,6d0,16383d0,100000d0, &
  1.5d0,0d0,0d0,6d0,16383d0,100000d0, &
  1d0,1.5d0,40d0,6d0,16383d0,100000d0, &
  20d0,65535d0,0d0,6d0,16383d0,100000d0, &
  50d0,0d0,0d0,6d0,16383d0,100000d0, &
  1d0,20d0,0d0,6d0,16383d0,100000d0, &
  90d0,0d0,0d0,6d0,16383d0,100000d0, &
  2d0,25d0,4000d0,0d0,0d0,100000d0, &
  4000d0,20d0,0d0,0d0,0d0,100000d0, &
  4000d0,0d0,0d0,0d0,0d0,100000d0, &
  1000d0,0d0,0d0,6d0,16383d0,100000d0, &
  2000d0,0d0,0d0,0d0,0d0,100000d0, &
  1d0,0d0,0d0,6d0,16383d0,100000d0, &
  1000d0,0d0,0d0,0d0,0d0,100000d0, &
  43d0,10d0,0d0,0d0,0d0,100000d0, &
  75d0,45d0,0d0,0d0,0d0,100000d0, &
  95d0,11d0,0d0,0d0,0d0,100000d0, &
  44d0,0d0,0d0,0d0,0d0,100000d0, &
  1d0,1d0,0d0,6d0,16383d0,100000d0, &
  1d0,0d0,0d0,6d0,16383d0,100000d0, &
  150d0,0d0,0d0,6d0,16383d0,100000d0, &
  1d0,0d0,0d0,6d0,16383d0,100000d0, &
  1d0,1d0,0d0,6d0,16383d0,100000d0, &
  60d0,0d0,0d0,6d0,16383d0,100000d0, &
  1d0,0d0,0d0,6d0,16383d0,100000d0, &
  3000d0,0d0,0d0,6d0,16383d0,100000d0, &
  1.5d0,1.5d0,0d0,6d0,16383d0,100000d0, &
  24d0,45d0,0d0,6d0,16383d0,100000d0, &
  20d0,0d0,0d0,6d0,16383d0,100000d0, &
  80d0,0d0,0d0,0d0,0d0,100000d0, &
  60d0,0d0,0d0,0d0,0d0,100000d0, &
  1d0,0d0,0d0,6d0,16383d0,100000d0, &
  270d0,0d0,0d0,0d0,0d0,100000d0, &
  1d0,1d0,0d0,0d0,0d0,100000d0, &
  0.1d0,44d0,0d0,6d0,16383d0,100000d0, &
  1000d0,0d0,0d0,6d0,16383d0,100000d0, &
  4d0,0.95d0,40d0,6d0,16383d0,100000d0, &
  80d0,0d0,0d0,6d0,16383d0,100000d0, &
  10000d0,0d0,0d0,6d0,16383d0,100000d0, &
  1d0,0d0,0d0,6d0,16383d0,100000d0, &
  12d0,0d0,0d0,6d0,16383d0,100000d0, &
  500d0,10d0,1000d0,6d0,16383d0,100000d0, &
  50d0,40d0,0d0,6d0,16383d0,100000d0, &
  1d0,0d0,0d0,6d0,16383d0,100000d0, &
  1000d0,0d0,0d0,6d0,16383d0,100000d0, &
  10000d0,0d0,0d0,6d0,16383d0,100000d0, &
  5d0,0d0,0d0,6d0,16383d0,100000d0],[6,51])
 real(c_double),parameter::scenario_step(6,51)=reshape([ &
  0.5d0,0d0,0d0,1d0,1d0,1d0, &
  0.05d0,0d0,0d0,0d0,0d0,1d0, &
  5d0,0.001d0,1d0,0d0,0d0,1d0, &
  0.05d0,0d0,0d0,1d0,1d0,1d0, &
  0.05d0,0d0,0d0,1d0,1d0,1d0, &
  0.05d0,0d0,0d0,1d0,1d0,1d0, &
  0.05d0,0.1d0,1d0,1d0,1d0,1d0, &
  0.5d0,250d0,0d0,1d0,1d0,1d0, &
  1d0,0d0,0d0,1d0,1d0,1d0, &
  0.05d0,1d0,0d0,1d0,1d0,1d0, &
  1d0,0d0,0d0,1d0,1d0,1d0, &
  0.05d0,1d0,60d0,0d0,0d0,1d0, &
  60d0,0.5d0,0d0,0d0,0d0,1d0, &
  60d0,0d0,0d0,0d0,0d0,1d0, &
  10d0,0d0,0d0,1d0,1d0,1d0, &
  25d0,0d0,0d0,0d0,0d0,1d0, &
  0.05d0,0d0,0d0,1d0,1d0,1d0, &
  10d0,0d0,0d0,0d0,0d0,1d0, &
  1d0,1d0,0d0,0d0,0d0,1d0, &
  1d0,1d0,0d0,0d0,0d0,1d0, &
  1d0,2d0,0d0,0d0,0d0,1d0, &
  1d0,0d0,0d0,0d0,0d0,1d0, &
  0.05d0,0.05d0,0d0,1d0,1d0,1d0, &
  0.05d0,0d0,0d0,1d0,1d0,1d0, &
  5d0,0d0,0d0,1d0,1d0,1d0, &
  0.05d0,0d0,0d0,1d0,1d0,1d0, &
  0.05d0,0.05d0,0d0,1d0,1d0,1d0, &
  1d0,0d0,0d0,1d0,1d0,1d0, &
  0.02d0,0d0,0d0,1d0,1d0,1d0, &
  50d0,0d0,0d0,1d0,1d0,1d0, &
  0.05d0,0.05d0,0d0,1d0,1d0,1d0, &
  0.5d0,1d0,0d0,1d0,1d0,1d0, &
  1d0,0d0,0d0,1d0,1d0,1d0, &
  1d0,0d0,0d0,0d0,0d0,1d0, &
  1d0,0d0,0d0,0d0,0d0,1d0, &
  0.1d0,0d0,0d0,1d0,1d0,1d0, &
  90d0,0d0,0d0,0d0,0d0,1d0, &
  1d0,1d0,0d0,0d0,0d0,1d0, &
  0.005d0,1d0,0d0,1d0,1d0,1d0, &
  1d0,0d0,0d0,1d0,1d0,1d0, &
  1d0,0.05d0,1d0,1d0,1d0,1d0, &
  5d0,0d0,0d0,1d0,1d0,1d0, &
  10d0,0d0,0d0,1d0,1d0,1d0, &
  0.05d0,0d0,0d0,1d0,1d0,1d0, &
  1d0,0d0,0d0,1d0,1d0,1d0, &
  20d0,1d0,50d0,1d0,1d0,1d0, &
  1d0,1d0,0d0,1d0,1d0,1d0, &
  0.02d0,0d0,0d0,1d0,1d0,1d0, &
  5d0,0d0,0d0,1d0,1d0,1d0, &
  10d0,0d0,0d0,1d0,1d0,1d0, &
  0.2d0,0d0,0d0,1d0,1d0,1d0],[6,51])
 real(c_double),save::scenario_bank(6,51)=scenario_defaults
contains
! @nativefn NR2-M04-F0001
 logical function scenario_available(code)result(ok)
  integer,intent(in)::code
  ok=code>=0.and.code<=51.and.code/=21
 end function
! @nativefn NR2-M04-F0002
 logical function parameters_valid(code,p)result(ok)
  integer,intent(in)::code
  real(c_double),intent(in)::p(6)
  ok=.false.;if(code<1.or.code>51)return
  if(any(.not.ieee_is_finite(p)))return
  if(any(abs(p*1000-anint(p*1000))>1d-6))return
  if(any(p(4:6)/=anint(p(4:6))))return
  if(code==37.and.mod(nint(p(1)),90)/=0)return
  select case(code)
  case(3,7,41)
   if(p(3)/=anint(p(3)))return
   if(code==41.and.p(1)/=anint(p(1)))return
  case(19,38)
   if(any(p(1:2)/=anint(p(1:2))))return
  case(20,21,39,46,47)
   if(p(2)/=anint(p(2)))return
  case(22,45)
   if(p(1)/=anint(p(1)))return
  end select
  if(any(p<scenario_min(:,code)).or.any(p>scenario_max(:,code)))return
  ok=.true.
 end function
! @nativefn NR2-M04-F0003
 integer function well_position(w,ne,nr)result(pos)
  integer,intent(in)::w,ne,nr
  if(w<=ne*nr)then
   pos=((w-1)/nr)*24+mod(w-1,nr)
  else
   pos=(w-ne*nr+3)*24+4
  end if
 end function
! @nativefn NR2-M04-F0004
 integer function recorded_position(code,p,w,ne,nr)result(pos)
  integer,intent(in)::code,w,ne,nr
  real(c_double),intent(in)::p(6)
  pos=well_position(w,ne,nr)
  if(code==37.and.nint(p(1))==180)pos=383-pos
 end function
! @nativefn NR2-M04-F0005
 logical function affects(code,p,w,ne,nr)result(yes)
  integer,intent(in)::code,w,ne,nr
  real(c_double),intent(in)::p(6)
  integer::scope,mask,pos,row,col
  yes=.true.;if(code==0)return
  mask=nint(p(5));if(mask>0)then
   yes=btest(mask,w-1);return
  end if
  scope=nint(p(4));if(scope==0)scope=default_scopes(code)
  pos=well_position(w,ne,nr);row=pos/24;col=mod(pos,24)
  select case(scope)
  case(2)
   yes=w<=ne*nr
  case(3)
   yes=w>ne*nr
  case(4)
   yes=row==0.or.row==15.or.col==0.or.col==23
  case(5)
   yes=row<4.and.col<4
  case(6)
   yes=w==ne*nr+2.or.w==ne*nr+3
  end select
 end function
! @nativefn NR2-M04-F0006
 subroutine edit_parameter(code,param,direction)
  integer,intent(in)::code,param,direction
  real(c_double)::v
  if(code<1.or.code>51.or.param<1.or.param>6)return
  v=scenario_bank(param,code)+direction*scenario_step(param,code)
  scenario_bank(param,code)=max(scenario_min(param,code),min(scenario_max(param,code),v))
 end subroutine
end module
