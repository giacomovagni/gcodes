#
fs = function(v, new_max = 100, new_min = 0) {
  old_max = max(v)
  old_min = min(v)
  out = (new_max - new_min) / (old_max - old_min) * (v - old_min) + new_min
  return(out)
}
#