function[newval]=delta(val,plusminus) 
if plusminus
  newval = val + val/20;
else
  newval = val - val/20;
end 
end
