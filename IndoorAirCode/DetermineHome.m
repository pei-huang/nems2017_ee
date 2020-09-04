function [home_type] = DetermineHome(homeprojec,year)
%DETERMINEHOME Returns a 1 for an old home or 2 for a new home based on the
%current year of the model

home_rand = rand();

% Read estimate for the year 
pct_old = homeprojec.Pct_Old(year);

if home_rand < pct_old
    home_type = 1; % meaning old
else 
    home_type = 2; % meaning new
end

end

