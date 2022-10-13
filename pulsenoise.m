function [noise] = pulsenoise(len, power_w, power_z)
    n = 2;
    pr = 0.1;
    b = binornd(n, p, [len, 1]);
    w = sqrt(power_z) * randn(len, 1);
    z = sqrt(power_z,) * randn(len, 1);
    noise = b .* w + z;
end
