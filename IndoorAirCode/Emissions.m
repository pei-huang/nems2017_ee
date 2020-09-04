function [E] = Emissions(household_size,year)
%EMISSIONS Generate the emission rate for a given home (ug/hr)

% Percent of electric and natural gas stoves
pct_elect = [0.613798637 0.615448478 0.617412431 0.619319206	0.621393877	0.623568956	0.625760334	0.627968106	0.63018522	0.632400426	0.634568524	0.636678632	0.638707468	0.64064554	0.642507589	0.644314862	0.646069753	0.647785571	0.649443151	0.651048523	0.652635233	0.654219295	0.655803042	0.657358984	0.658882902	0.660381031	0.661832892	0.663246415	0.66461936	0.665966822	0.667293051	0.668594631	0.669862835	0.671100843	0.67231101	0.673486268	0.674638486	0.675767782];
pct_nat = [0.953348371	0.953863445	0.954600807	0.955290261	0.955992639	0.956690588	0.957377477	0.958059479	0.958726938	0.959371717	0.959996076	0.960604288	0.96119834	0.961790272	0.96234653	0.962872968	0.963372371	0.963846281	0.964284043	0.96471934	0.96514824	0.965571036	0.965988109	0.966398308	0.966802418	0.96719733	0.967581905	0.967961491	0.968335884	0.968705831	0.969071837	0.96943408	0.969791719	0.970138898	0.970471645	0.970788616	0.971090574	0.971380698];


% Determine stove usage
stove_rand = rand();
if stove_rand < 0.115
    stove_use = 0;
elseif stove_rand >= 0.115 && stove_rand < 0.258
    stove_use = 1+(3-1)*rand();
elseif stove_rand >= 0.258 && stove_rand < 0.685
    stove_use = 4+(7-4)*rand();
elseif stove_rand >= 0.685 && stove_rand < 0.887
    stove_use = 8+(14-8)*rand();
else 
    stove_use = 15;
end
       
% Determine oven usage
oven_rand = rand();
if oven_rand < 0.243
    oven_use = 0;
elseif oven_rand >= 0.243 && oven_rand < 0.728
    oven_use = 1+(3-1)*rand();
elseif oven_rand >= 0.728 && oven_rand < 0.968
    oven_use = 4+(7-4)*rand();
elseif oven_rand >= 0.968 && oven_rand < 0.998
    oven_use = 8+(14-8)*rand();
else 
    oven_use = 15;
end

% Determine washing machine usage
mw_rand = rand();
if mw_rand < 0.051
    mw_use = 0;
elseif mw_rand >= 0.051 && oven_rand < 0.140
    mw_use = 1+(3-1)*rand();
elseif mw_rand >= 0.140 && oven_rand < 0.471
    mw_use = 4+(7-4)*rand();
elseif mw_rand >= 0.471 && oven_rand < 0.710
    mw_use = 8+(14-8)*rand();
else 
    mw_use = 15;
end

% Toaster usage
toast_rand = rand();
if toast_rand < 0.6
    toast_use = 3; % 60% of households use a toaster more than once a week
else 
    toast_use = 0;
end

% Determine washing machine usage
wash_rand = rand();
if wash_rand < 0.187
    wash_use = 0;
elseif wash_rand >= 0.187 && oven_rand < 0.694
    wash_use = 1+(3-1)*rand();
elseif wash_rand >= 0.694 && oven_rand < 0.921
    wash_use = 4+(7-4)*rand();
elseif wash_rand >= 0.921 && oven_rand < 0.991
    wash_use = 8+(14-8)*rand();
else 
    wash_use = 15;
end

% Determine stove/oven emission rate (ug/hr) Hu et al
stove_type_rand = rand();
if stove_type_rand < pct_elect(year)
    ER_stoven = 1.1e4;
elseif stove_type_rand >= pct_elect(year) && stove_type_rand < pct_nat(year)
    ER_stoven = 3.8e4;
else 
    ER_stoven = 1.2e4; % Propane emission rate from  Joo et al.
end

% Microwave emission rate (ug/hr) Hu et al
ER_mw = 3.2e2;
% Toaster emission rate (ug/hr) He et al
ER_toast = 6.6e3;
% Washing machine emission rate (ug/hr) He et al
ER_wash = 7.2e3;
% Shower emission rate (ug/hr) He et al
ER_shower = 2.4e3;
% Vacuuming emission rate (ug/hr) He et al
ER_vac = 4.2e3;

% Typical applicance usage
oven_time = 60/60; % hour
stove_time = 60/60; % hour
mw_time = 5/60; % 5 mins
toast_time = 2/60; % 2 mins
wash_time = 30/60; % Half hour
shower_time = 15/60*7;
vac_time = 30/60; % Half hour

% Sum individual sources
E_stoven = ER_stoven*stove_use*stove_time+ER_stoven*oven_use*oven_time;
E_mw = ER_mw*mw_use*mw_time;
E_toast = ER_toast*toast_use*toast_time;
E_wash = ER_wash*wash_use*wash_time;
E_shower = ER_shower*household_size*shower_time; 
E_vac = ER_vac*vac_time;

% Add up all of the sources and divide by hours in a week
E = (E_stoven+E_mw+E_toast+E_wash+E_shower+E_vac)/168;

end

