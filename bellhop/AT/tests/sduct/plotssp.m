cmat = [    0.0, 1497.0 ;
   250.0, 1502.0 ;
   300.0, 1485.0 ;
   375.0, 1478.0 ;
   425.0, 1477.0 ;
   500.0, 1476.0 ;
   600.0, 1476.5 ;
   700.0, 1477.0 ;
   810.0, 1478.0 ;
   900.0, 1479.0 ;
  1000.0, 1480.0 ;
  1100.0, 1481.0 ;
  1180.0, 1482.0 ;
  1340.0, 1484.0 ;
  1600.0, 1487.0 ;
  1800.0, 1490.0 ;
  2500.0, 1498.7 ;
  3000.0, 1506.8 ;
  4000.0, 1523.9 ]

figure; hh = plot( cmat( :, 2 ), cmat( :, 1 ) )

view( 0, -90 );    
xlabel( 'Sound Speed (m/s)' )
ylabel( 'Depth (m)' )

foo = set( hh, 'LineWidth', 3 );

