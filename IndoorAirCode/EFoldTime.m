function [efoldT] = EFoldTime(C_ss,a_inf,a_nat,P,C0,house_vol,k_dep,a_recirc,eff_filt,E_back)

a_tot = a_inf + a_nat;

dt = 1/60; % hr timestep increment (currently at 1 min). 
run_time = 12; % hours
timesteps = run_time/dt; % number of increments for the program 

time = 1:(timesteps+1);
time = (time - 1)*dt; % Start at time equals 0 rather than time equals one.

E(1:(timesteps+1)) = 0;
E(1:(1/dt)-1) = 3.8e4; 
E((1/dt):(timesteps+1)) = E_back; % then back to a lower baseline emission value

C = zeros(1,timesteps+1);
C(1) = C_ss; % Assume the home starts at SS conc 

    for i = 1:timesteps
        dCdt = a_inf*P*C0+a_nat*C0+a_recirc*C(i)*(1-eff_filt)+E(i)/house_vol-a_tot*C(i)-k_dep*C(i)-a_recirc*C(i); % ug/m3/hr
        C(i+1) = C(i) + dCdt*dt;              
    end

% Find the e-folding time of the home to this step change
[M,I] = max(C);

diff = M-C_ss;
efold_C = diff/exp(1)+C_ss; % This is the concentration value corresponding to the e-folding time

    for i = (1/dt):(timesteps+1)
        if C(i) < efold_C
            efoldT = time(i)-time((1/dt));
            break
        end
    end

end