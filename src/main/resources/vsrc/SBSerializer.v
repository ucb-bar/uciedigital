module SBSerializerBlackBox #(
    parameter WIDTH = 128,
    parameter WIDTH_W = $clog2(WIDTH)
) (

    input clk,
    input rst,
    input [WIDTH - 1:0] in_data,
    output out_data

    );

endmodule