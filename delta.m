function[newval]=delta(val,plusminus) 
if plusminus
  newval = val + val/20.0;
else
  newval = val - val/20.0;
end
endfunction
