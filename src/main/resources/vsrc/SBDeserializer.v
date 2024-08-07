module SBDeserializerBlackBox #(
    parameter WIDTH = 128,
    parameter WIDTH_W = $clog2(WIDTH)
) (
    input clk,
    input rst,
    input in_data,
    output [WIDTH - 1:0] out_data,
    output out_data_valid

);

reg [WIDTH_W-1:0] counter;
reg [WIDTH-1:0] data_reg;
reg receiving;
wire recvDone;

assign out_data = data_reg;
assign recvDone = counter == (WIDTH - 1);
assign out_data_valid = !receiving;

always @(negedge clk or posedge rst) begin
    if (rst) begin
        counter <= 0;
        receiving <= 1'b1;
    end else begin
        if (recvDone) begin
            counter <= 0;
            receiving <= 1'b0;
        end else begin
            counter <= counter + 1'b1;
            receiving <= 1'b1;
        end

        // if (out_data_valid && out_data_ready) begin
        //     receiving <= 1'b1;
        // end

        data_reg[counter] <= in_data;

    end

end

endmodule