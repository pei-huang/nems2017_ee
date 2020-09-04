function [household_size] = HouseholdSize()
%HOUSEHOLDSIZE Summary of this function goes here
%   Detailed explanation goes here

housesize_rand = rand();
if housesize_rand < 0.243
    household_size = 1;
elseif housesize_rand >= 0.243 && housesize_rand < 0.604
    household_size = 2;
elseif housesize_rand >= 0.604 && housesize_rand < 0.768
    household_size = 3;
elseif housesize_rand >= 0.768 && housesize_rand < 0.899
    household_size = 4;
else 
    household_size = 5;
end

end

