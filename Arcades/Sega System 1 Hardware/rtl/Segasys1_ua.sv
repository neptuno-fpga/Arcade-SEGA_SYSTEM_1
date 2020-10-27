//============================================================================
//
//  Multicore 2+ Top by Victor Trucco
//
//============================================================================
//
//============================================================================
//
//  unamiga adapted by Delgrom
//
//============================================================================
`default_nettype none

module Segasys1_ua(
    // Clocks
    input wire  clock_50_i,

    // Buttons
    //input wire [4:1]    btn_n_i,

    // SRAM 
    output wire [18:0]sram_addr_o  = 20'b00000000000000000000,
    inout wire  [7:0]sram_data_io,
    output wire sram_we_n_o     = 1'b1,
    output wire sram_oe_n_o     = 1'b1,
        
    // SDRAM
    output [12:0] SDRAM_A,
    output  [1:0] SDRAM_BA,
    inout  [15:0] SDRAM_DQ,
    output        SDRAM_DQMH,
    output        SDRAM_DQML,
    output        SDRAM_CKE,
    output        SDRAM_nCS,
    output        SDRAM_nWE,
    output        SDRAM_nRAS,
    output        SDRAM_nCAS,
    output        SDRAM_CLK,

    // PS2
    inout wire  ps2_clk_io        = 1'bz,
    inout wire  ps2_data_io       = 1'bz,
    inout wire  ps2_mouse_clk_io  = 1'bz,
    inout wire  ps2_mouse_data_io = 1'bz,

    // SD Card
    output wire sd_cs_n_o         = 1'bZ,
    output wire sd_sclk_o         = 1'bZ,
    output wire sd_mosi_o         = 1'bZ,
    input wire  sd_miso_i,
	
    // Joysticks
   input wire  joy1_up_i,
   input wire  joy1_down_i,
   input wire  joy1_left_i,
   input wire  joy1_right_i,
   input wire  joy1_p6_i,
   input wire  joy1_p9_i,
   input wire  joy2_up_i,
   input wire  joy2_down_i,
   input wire  joy2_left_i,
   input wire  joy2_right_i,
   input wire  joy2_p6_i,
   input wire  joy2_p9_i,
    output wire joy_p7_o          = 1'b1,

    // Audio
    output      AUDIO_L,
    output      AUDIO_R,
    //input wire  ear_i,
    //output wire mic_o             = 1'b0,

    // VGA
    output  [4:0] VGA_R,
    output  [4:0] VGA_G,
    output  [4:0] VGA_B,
    output        VGA_HS,
    output        VGA_VS,

    //STM32
    input wire  stm_tx_i,
    output wire stm_rx_o,
    //output wire stm_rst_o           = 1'bz, // '0' to hold the microcontroller reset line, to free the SD card
   
    input         SPI_SCK,
    output        SPI_DO,
    input         SPI_DI,
    input         SPI_SS2,
    //output wire   SPI_nWAIT        = 1'b1, // '0' to hold the microcontroller data streaming

    //inout [31:0] GPIO,

    output LED                    = 1'b1 // '0' is LED on
);


//---------------------------------------------------------
//-- MC2+ defaults
//---------------------------------------------------------
//assign GPIO = 32'bzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;
//assign stm_rst_o    = 1'bZ;
assign stm_rx_o = 1'bZ;

//no SRAM for this core
assign sram_we_n_o  = 1'b1;
assign sram_oe_n_o  = 1'b1;

//all the SD reading goes thru the microcontroller for this core
assign sd_cs_n_o = 1'bZ;
assign sd_sclk_o = 1'bZ;
assign sd_mosi_o = 1'bZ;


// wire joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i, joy1_p6_i, joy1_p9_i;
// wire joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i, joy2_p6_i, joy2_p9_i;

// joydecoder joystick_serial  (
    // .clk          ( clk_sys ), 	
    // .joy_data     ( joy_data_i ),
    // .joy_clk      ( joy_clock_o ),
    // .joy_load     ( joy_load_o ),
	 // .clock_locked ( pll_locked ),

    // .joy1up       ( joy1_up_i ),
    // .joy1down     ( joy1_down_i ),
    // .joy1left     ( joy1_left_i ),
    // .joy1right    ( joy1_right_i ),
    // .joy1fire1    ( joy1_p6_i ),
    // .joy1fire2    ( joy1_p9_i ),

    // .joy2up       ( joy2_up_i ),
    // .joy2down     ( joy2_down_i ),
    // .joy2left     ( joy2_left_i ),
    // .joy2right    ( joy2_right_i ),
    // .joy2fire1    ( joy2_p6_i ),
    // .joy2fire2    ( joy2_p9_i )
// ); 


// ROM Data Pump
reg [7:0] pump_s = 8'b11111111;
PumpSignal PumpSignal (clk_sys, ~pll_locked, ioctl_downl, pump_s);

//-----------------------------------------------------------------

localparam CONF_STR = {
    "P,CORE_NAME.dat;",
//    "P,Wonder Boy.dat;",
    "O34,Scanlines,Off,25%,50%,75%;",
    "O5,Blend,Off,On;",
	 "O6,Scandoubler,On,Off;",
    "T0,Reset;",
    "V,v1.0."
};

assign SDRAM_CKE = 1;
assign AUDIO_R = AUDIO_L;
assign LED = ~ioctl_downl;

wire        rotate    = status[2];
wire  [1:0] scanlines = status[4:3];
wire        blend = status[5];

wire  [7:0] INP0 = ~{m_left, m_right,m_up, m_down,1'b0,m_fireB,m_fireA,m_fireC};
wire  [7:0] INP1 = ~{m_left2,m_right2,m_up2, m_down2,1'b0,m_fire2B,m_fire2A,m_fire2C};
//wire  [7:0] INP2 = ~{2'b00,m_two_players, m_one_player,3'b000, m_coin1};
wire  [7:0] INP2 = ~{2'b00,btn_two_players, btn_one_player,3'b000, btn_coin};

wire  [7:0] DSW0 = status[15: 8];
wire  [7:0] DSW1 = status[23:16];

wire  [6:0] core_mod;  // [0]=SYS1/SYS2,[1]=H/V,[2]=H256/H240
wire  [1:0] orientation = { 1'b0, core_mod[1] };


wire clk_sys, sdram_clk;
wire pll_locked;
pll_mist pll(
    .inclk0(clock_50_i),
    .c0(clk_sys),//48
    .c1(sdram_clk),//96
    .c2(SDRAM_CLK),
    .locked(pll_locked)
    );
//assign SDRAM_CLK = sdram_clk;


wire [31:0] status;
wire  [1:0] buttons;
wire  [1:0] switches;
wire  [7:0] joystick_0;
wire  [7:0] joystick_1;
wire        key_pressed;
wire        key_strobe;
wire  [7:0] key_code;
wire        scandoublerD;
wire        ypbpr;
wire        no_csync;
/*
user_io #(
    .STRLEN(($size(CONF_STR)>>3)))
user_io(
    .clk_sys        (clk_sys        ),
    .conf_str       (CONF_STR       ),
    .SPI_CLK        (SPI_SCK        ),
    .SPI_SS_IO      (CONF_DATA0     ),
    .SPI_MISO       (SPI_DO         ),
    .SPI_MOSI       (SPI_DI         ),
    .buttons        (buttons        ),
    .switches       (switches       ),
    .scandoubler_disable (scandoublerD ),
    .ypbpr          (ypbpr          ),
    .core_mod       (core_mod       ),
    .no_csync       (no_csync       ),
    .key_strobe     (key_strobe     ),
    .key_pressed    (key_pressed    ),
    .key_code       (key_code       ),
    .joystick_0     (joystick_0     ),
    .joystick_1     (joystick_1     ),
    .status         (status         )
    );
*/
wire [15:0] audio;
wire [15:0] rom_addr;
wire [15:0] rom_do;
wire [15:0] spr_rom_addr;
wire [15:0] spr_rom_do;
wire [12:0] snd_rom_addr;
wire [15:0] snd_rom_do;
wire [13:0] tile_rom_addr;
wire [23:0] tile_rom_do;
wire        ioctl_downl;
wire  [7:0] ioctl_index;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;

data_io #(
    .STRLEN(($size(CONF_STR)>>3)))
data_io(
    .clk_sys       ( clk_sys      ),
    .SPI_SCK       ( SPI_SCK      ),
    .SPI_SS2       ( SPI_SS2      ),
    .SPI_DI        ( SPI_DI       ),
    .SPI_DO        ( SPI_DO       ),
    
    .data_in       ( pump_s & osd_s ),
    .conf_str      ( CONF_STR     ),
    .status        ( status       ),
    .core_mod       (core_mod       ),

    .ioctl_download( ioctl_downl  ),
    .ioctl_index   ( ioctl_index  ),
    .ioctl_wr      ( ioctl_wr     ),
    .ioctl_addr    ( ioctl_addr   ),
    .ioctl_dout    ( ioctl_dout   )
);

reg port1_req, port2_req;
wire [24:0] tl_ioctl_addr = ioctl_addr - 18'h20000;
sdram #(96) sdram(
    .*,
    .init_n        ( pll_locked   ),
    .clk           ( sdram_clk    ),

    // port1 used for main + sound CPUs
    .port1_req     ( port1_req    ),
    .port1_ack     ( ),
    .port1_a       ( ioctl_addr[23:1] ),
    .port1_ds      ( {ioctl_addr[0], ~ioctl_addr[0]} ),
    .port1_we      ( ioctl_downl ),
    .port1_d       ( {ioctl_dout, ioctl_dout} ),
    .port1_q       ( ),

    .cpu1_addr     ( ioctl_downl ? 16'hffff : {1'b0, rom_addr[15:1]}), // offset 0
    .cpu1_q        ( rom_do ),
    .cpu2_addr     ( ioctl_downl ? 16'hffff : (16'h6000 + snd_rom_addr[12:1]) ), // offset c000
    .cpu2_q        ( snd_rom_do ),
    .cpu3_addr     ( ioctl_downl ? 16'hffff : (16'h8000 + spr_rom_addr[15:1]) ), // offset 10000
    .cpu3_q        ( spr_rom_do ),

    // port2 for backround tiles
    .port2_req     ( port2_req ),
    .port2_ack     ( ),
    .port2_a       ( tl_ioctl_addr[23:1] ),
    .port2_ds      ( {tl_ioctl_addr[0], ~tl_ioctl_addr[0]} ),
    .port2_we      ( ioctl_downl ),
    .port2_d       ( {ioctl_dout, ioctl_dout} ),
    .port2_q       ( ),

    .sp_addr       ( ioctl_downl ? 15'h7fff : tile_rom_addr ),
    .sp_q          ( tile_rom_do )
);

always @(posedge clk_sys) begin
    reg        ioctl_wr_last = 0;
    ioctl_wr_last <= ioctl_wr;
    if (ioctl_downl) begin
        if (~ioctl_wr_last && ioctl_wr) begin
            port1_req <= ~port1_req;
            port2_req <= ~port2_req;
        end
    end
end

reg reset = 1;
reg rom_loaded = 0;
always @(posedge sdram_clk) begin
    reg ioctl_downlD;
    ioctl_downlD <= ioctl_downl;

    if (ioctl_downlD & ~ioctl_downl) rom_loaded <= 1;
    //reset <= status[0] | ~btn_n_i[4] | ~rom_loaded;
	reset <= status[0] | ~rom_loaded;
end

SEGASYSTEM1 System1_Top(
    .clk48M(clk_sys),
    .reset(reset),

    .INP0(INP0),
    .INP1(INP1),
    .INP2(INP2),

    .DSW0(DSW0),
    .DSW1(DSW1),
    .PH(HPOS),
    .PV(VPOS),
    .PCLK_EN(PCLK_EN),
    .POUT(POUT),

    .cpu_rom_addr(rom_addr),
    .cpu_rom_do( rom_addr[0] ? rom_do[15:8] : rom_do[7:0] ),

    .snd_rom_addr(snd_rom_addr),
    .snd_rom_do(snd_rom_addr[0] ? snd_rom_do[15:8] : snd_rom_do[7:0] ),

    .spr_rom_addr(spr_rom_addr),
    .spr_rom_do(spr_rom_addr[0] ? spr_rom_do[15:8] : spr_rom_do[7:0] ),

    .tile_rom_addr(tile_rom_addr),
    .tile_rom_do(tile_rom_do),

    .ROMCL(clk_sys),
    .ROMAD(ioctl_addr[17:0]),
    .ROMDT( ioctl_dout ),
    .ROMEN( ioctl_wr ),
    .SOUT(audio)
);

wire        PCLK_EN;
wire  [8:0] HPOS,VPOS;
wire  [7:0] POUT;
wire  [7:0] HOFFS = 8'd16;
wire  [7:0] VOFFS = 0;
wire        hs, vs;
wire  [2:0] g, r;
wire  [1:0] b;

HVGEN hvgen
(
    .HPOS(HPOS),.VPOS(VPOS),.CLK(clk_sys),.PCLK_EN(PCLK_EN),.iRGB(POUT),
    .oRGB({b,g,r}),.HBLK(),.VBLK(),.HSYN(hs),.VSYN(vs),
    .H240(core_mod[2]),.HOFFS(HOFFS),.VOFFS(VOFFS)
);


assign scandoublerD = ~status[6] ^ direct_video;

mist_video #(.COLOR_DEPTH(3), .SD_HCNT_WIDTH(10)) mist_video(
    .clk_sys        ( clk_sys          ),
    .SPI_SCK        ( SPI_SCK          ),
    .SPI_SS3        ( SPI_SS2          ),
    .SPI_DI         ( SPI_DI           ),
    .R              ( r                ),
    .G              ( g                ),
    .B              ( {b, b[1]}        ),
    .HSync          ( hs               ),
    .VSync          ( vs               ),
    .VGA_R          ( VGA_R            ),
    .VGA_G          ( VGA_G            ),
    .VGA_B          ( VGA_B            ),
    .VGA_VS         ( VGA_VS           ),
    .VGA_HS         ( VGA_HS           ),
    .ce_divider     ( 1'b0             ),
    .blend          ( blend            ),
    .rotate         ( {1'b0, rotate}   ),
    .scandoubler_disable(scandoublerD  ),
    .scanlines      ( scanlines        ),
    .osd_enable     ( osd_enable )
);

dac #(
    .C_bits(16)) 
dac(
    .clk_i(clk_sys),
    .res_n_i(1),
    .dac_i(audio),
    .dac_o(AUDIO_L)
);


reg [2:0]clk_div;
always @(posedge clk_sys) begin
    clk_div <= clk_div + 1'b1;
end

wire m_up, m_down, m_left, m_right, m_fireA, m_fireB, m_fireC, m_fireD, m_fireE, m_fireF, m_fireG, m_fireH, m_fireI;
wire m_up2, m_down2, m_left2, m_right2, m_fire2A, m_fire2B, m_fire2C, m_fire2D, m_fire2E, m_fire2F, m_fire2G, m_fire2H, m_fire2I;
wire m_tilt, m_coin1, m_coin2, m_coin3, m_coin4, m_one_player, m_two_players, m_three_players, m_four_players;

wire m_right4, m_left4, m_down4, m_up4, m_right3, m_left3, m_down3, m_up3;

//wire btn_one_player  = ~btn_n_i[1] | m_one_player;
//wire btn_two_players = ~btn_n_i[2] | m_two_players;
//wire btn_coin        = ~btn_n_i[3] | m_coin1;
wire btn_one_player  = m_one_player | m_fireH; // start joy1
wire btn_two_players = m_two_players;
wire btn_coin        = m_coin1 | (m_fireH & m_fireC) | m_fireG; // start+c joy1, select joy1
wire kbd_intr;
wire [7:0] kbd_scancode;
wire [7:0] osd_s;

//get scancode from keyboard
io_ps2_keyboard keyboard 
 (
  .clk       ( clk_div[2] ),
  .kbd_clk   ( ps2_clk_io ),
  .kbd_dat   ( ps2_data_io ),
  .interrupt ( kbd_intr ),
  .scancode  ( kbd_scancode )
);

wire [15:0]joy1_s;
wire [15:0]joy2_s;
wire [8:0]controls_s;
wire osd_enable;
wire direct_video;
wire [1:0]osd_rotate;

//translate scancode to joystick
//kbd_joystick #( .OSD_CMD    ( 3'b011 ), .CLK_SPEED(6000)) k_joystick
kbd_joystick_ua #( .OSD_CMD    ( 3'b011 )) k_joystick
(
    .clk          ( clk_div[2] ),
    .kbdint       ( kbd_intr ),
    .kbdscancode  ( kbd_scancode ), 

    .joystick_0   ({ joy1_p9_i, joy1_p6_i, joy1_up_i, joy1_down_i, joy1_left_i, joy1_right_i }),
    .joystick_1   ({ joy2_p9_i, joy2_p6_i, joy2_up_i, joy2_down_i, joy2_left_i, joy2_right_i }),
      
    //-- joystick_0 and joystick_1 should be swapped
    .joyswap      ( 0 ),

    //-- player1 and player2 should get both joystick_0 and joystick_1
    .oneplayer    ( 1 ),

    //-- tilt, coin4-1, start4-1
    .controls     ( {m_tilt, m_coin4, m_coin3, m_coin2, m_coin1, m_four_players, m_three_players, m_two_players, m_one_player} ),

    //-- fire12-1, up, down, left, right

    .player1      ( {m_fireI, m_fireH,  m_fireG,  m_fireF, m_fireE, m_fireD, m_fireC, m_fireB, m_fireA, m_up, m_down, m_left, m_right} ),
    .player2      ( {m_fire2I, m_fire2H, m_fire2G, m_fire2F, m_fire2E, m_fire2D, m_fire2C, m_fire2B, m_fire2A, m_up2, m_down2, m_left2, m_right2} ),

    .direct_video ( direct_video ),
    .osd_rotate   ( osd_rotate ),

    //-- keys to the OSD
    .osd_o        ( osd_s ),
    .osd_enable   ( osd_enable ),

    //-- sega joystick
    .sega_clk     ( hs ),		
    .sega_strobe  ( joy_p7_o )
);

endmodule 
