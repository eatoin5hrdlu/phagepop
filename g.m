function xdot = g(x,t)
# SNR = [-10:5:30]; %in Db
  snr = 130;

  Dc = 1.0;
  h0 = 10^9;
  p = 0.4; # Population Density of Ho
  k1 = 0.4;
#k2 = awgn(0.4,snr,'measured');
  k2 = 0.4;
  k3 = 6.1;
  k4 = 0.2;
  kg = 0.5;

  k1 = delta(k1,1);
  k2 = delta(k2,1);
  k3 = delta(k3,1);
  k4 = delta(k4,1);
  kg = delta(kg,1);

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
