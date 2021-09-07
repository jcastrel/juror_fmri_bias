#!/bin/sh
#mask dilation for outline

#binarize the positive threshold map
fslmaths juror_scenario_thresh_pos_only.nii.gz -bin juror_scenario_thresh_pos_only_bin.nii.gz

#dilate the map
fslmaths juror_scenario_thresh_pos_only.nii.gz -bin -kernel 2D -dilM juror_scenario_thresh_pos_only_bin_dilated -odt short

#subtract the two maps
fslmaths juror_scenario_thresh_pos_only_bin_dilated.nii.gz -sub juror_scenario_thresh_pos_only_bin.nii.gz juror_scenario_thresh_pos_only_outline

