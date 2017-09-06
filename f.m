function xdot = f(x,t,kon)
# SNR = [-10:5:30]; %in Db
  snr = 130;
  global kon;
  global Dc;
  global h0;

  p = 0.4; # Population Density of Ho

  k1 = kon(1);
  k2 = kon(2);
  k3 = kon(3);
  k4 = kon(4);
  kg = kon(5);

  inhibit = 0.9;
% Each successive growth rate (slightly) inhibited
  kgp = kg*inhibit;
  kgpp = kgp*inhibit;
  kgppp = kgpp*inhibit;

  xdot(1) = Dc*h0 - Dc*x(1) - k1*p*x(1) + kg*x(1);
  xdot(2) = -Dc*x(2) + k1*x(1) + kgp*x(2) - k2*x(2);
  xdot(3) = -Dc*x(3) + kgpp*x(3) + k2*x(2) - k4*x(3);
  xdot(4) = -Dc*x(4) + kgppp*x(4) + k4*x(3);
  xdot(5) = -Dc*x(5) - k1*p*x(1) + k3*x(3);
  endfunction
