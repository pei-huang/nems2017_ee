function [a_inf,a_nat] = Ventilation(year,home_type,rel_a_inf,scen)

if home_type == 1
% For old homes 
    pd_nat = makedist('Normal','mu',0.2,'sigma',0.15);
    pd_inf = makedist('Normal','mu',0.54,'sigma',0.21);
else
    pd_nat = makedist('Normal','mu',0.16,'sigma',0.08);
    pd_inf = makedist('Normal','mu',0.24,'sigma',0.06);
end

a_inf = random(pd_inf)*rel_a_inf(home_type,year,scen);
a_nat = random(pd_nat);

% Safety measures
if a_inf < 0.1
    a_inf = 0.1;
end
if a_nat < 0
    a_nat = 0;
end

end

