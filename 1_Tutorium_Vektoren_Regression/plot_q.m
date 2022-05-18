clear()

% Create mash over beta_0 and beta_1
[beta_0,beta_1] = meshgrid(-5000:100:5000,-500:50:500);
%[beta_0,beta_1] = meshgrid(-8:0.05:0,0:0.05:5);

% Define x and y data vectors
x = transpose(1:20);
y = transpose([-3.15,2.52,-1.18,3.06,1.7,2.91,3.92,2.31,4.63,10.91,17.56,11.52,12.31,12.12,12.13,20.37,25.26,27.75,24.93,32.49]);

% Evaluate inner terms of q function
for i = 1:20
    b1_times_x(:,:,i) = beta_1 * x(i);
end

for i = 1:20
    b0_plus_beta1_times_x(:,:,i) = b1_times_x(:,:,i)+ beta_0;
end

for i = 1:20
    y_minus_beta_x(:,:,i) = y(i) - b0_plus_beta1_times_x(:,:,i);
end

for i = 1:20
    power_2(:,:,i) = y_minus_beta_x(:,:,i).^2;
end

% Evaluate q
q = sum(power_2,3);

% Plot q
surf(beta_0,beta_1,q);

qmin = min(q(:));
%hold on
%surf(beta_0,beta_1,qmin*ones(size(q)));