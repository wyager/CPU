`timescale 1ps/1ps


module main();

    initial begin
        reset_reg = 0;
        reset_reg = #1 1;
    end

    
    // clock
    wire clk;
    clock c0(clk);
    
    reg reset_reg;
    wire reset = reset_reg;

    wire halt = 0;
    wire output_valid = 0;
    wire [63:0] output_data;

    // Main_topEntity evaluator(clk, reset, halt, output_valid, output_data);
    
    always@(posedge clk) begin
        if (output_valid == 1) begin
            $display("%H", output_data);
        end else begin
            $display(".");
        end
    end

    always@(posedge clk) begin
        if (halt == 1) $finish;
    end

endmodule