library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.all;
 
entity i2c_master_tb is
GENERIC(
    sys_clk_freq     : INTEGER := 50_000_000);                                                 
  --PORT(
    --clk         : IN    STD_LOGIC;                             
    --reset_n     : IN    STD_LOGIC;                               
    --scl         : INOUT STD_LOGIC;                       
    --sda         : INOUT STD_LOGIC);                              
    --i2c_ack_err : OUT   STD_LOGIC;                                 
    --temperature : OUT   STD_LOGIC_VECTOR(15 DOWNTO 0)); 
end i2c_master_tb;
 
architecture behave of i2c_master_tb is
TYPE machine IS(start, write_data, read_data, output_result);
 SIGNAL state       : machine;                       
 SIGNAL   clk         : STD_LOGIC := '0';                              
 SIGNAL   reset_n     : STD_LOGIC := '1';                            
 SIGNAL   scl         : STD_LOGIC;                                 
 SIGNAL   sda         : STD_LOGIC;                                 
 SIGNAL   i2c_ack_err : STD_LOGIC;                                
 SIGNAL   temperature : STD_LOGIC_VECTOR(15 DOWNTO 0);  

  SIGNAL config      : STD_LOGIC_VECTOR(7 DOWNTO 0);  
  SIGNAL i2c_ena     : STD_LOGIC;                    
  SIGNAL i2c_addr    : STD_LOGIC_VECTOR(6 DOWNTO 0);  
  SIGNAL i2c_rw      : STD_LOGIC;                     
  SIGNAL i2c_data_wr : STD_LOGIC_VECTOR(7 DOWNTO 0);  
  SIGNAL i2c_data_rd : STD_LOGIC_VECTOR(7 DOWNTO 0);
  SIGNAL i2c_slave_data_rd : STD_LOGIC_VECTOR(7 DOWNTO 0);  
  SIGNAL i2c_busy    : STD_LOGIC;                     
  SIGNAL busy_prev   : STD_LOGIC;                   
  SIGNAL temp_data   : STD_LOGIC_VECTOR(15 DOWNTO 0); 
  SIGNAL   stretch2         : STD_LOGIC;

COMPONENT i2c_master IS
    GENERIC(
     input_clk : INTEGER;  --input clock speed from user logic in Hz
     bus_clk   : INTEGER); --speed the i2c bus (scl) will run at in Hz
    PORT(
     clk       : IN     STD_LOGIC;                    
     reset_n   : IN     STD_LOGIC;                    
     ena       : IN     STD_LOGIC;                   
     addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); 
     rw        : IN     STD_LOGIC;                    
     data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); 
     busy      : OUT    STD_LOGIC;                    
     data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); 
     ack_error : INOUT  STD_LOGIC;                    
     sda       : INOUT  STD_LOGIC;                    
     scl       : INOUT  STD_LOGIC);                   
  END COMPONENT;
  
  COMPONENT i2c_slave IS
    GENERIC(
     input_clk : INTEGER;  --input clock speed from user logic in Hz
     bus_clk   : INTEGER); --speed the i2c bus (scl) will run at in Hz
    PORT(
     clk       : IN     STD_LOGIC;                    
     reset_n   : IN     STD_LOGIC;                    
     ena       : IN     STD_LOGIC;                   
     rw        : IN     STD_LOGIC;                    
     data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0);                     
     data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0);                     
     sda       : INOUT  STD_LOGIC;                    
     scl       : INOUT  STD_LOGIC);                   
  END COMPONENT;
 

   begin --instance of master
  i2c_master_0:  i2c_master
    GENERIC MAP(input_clk => sys_clk_freq, bus_clk => 400_000)
    PORT MAP(clk => clk,
     reset_n => reset_n, 
     ena => i2c_ena, addr => i2c_addr,
     rw => i2c_rw, 
     data_wr => i2c_data_wr, 
     busy => i2c_busy,
     data_rd => i2c_data_rd, 
     ack_error => i2c_ack_err,
     sda => sda,
     scl => scl);
     
     i2c_slave_0:  i2c_slave --instance of slave
    GENERIC MAP(input_clk => sys_clk_freq, bus_clk => 400_000)
    PORT MAP(clk => clk,
     reset_n => reset_n, 
     ena => i2c_ena,
     rw => i2c_rw, 
     data_wr => i2c_data_wr, 
     data_rd => i2c_slave_data_rd, 
     sda => sda,
     scl => scl);

 
  
  clk <= not clk after 20 ns;
   

PROCESS(clk, reset_n)
    VARIABLE busy_cnt : INTEGER RANGE 0 TO 4 := 0;                
  BEGIN
    IF(reset_n = '0') THEN                                     
      i2c_ena <= '0';                  
      busy_cnt := 0;                    
      temperature <= (OTHERS => '0');    
      state <= start;                     
    ELSIF(clk'EVENT AND clk = '1') THEN  
      CASE state IS                       
        WHEN start =>
            state <= write_data;

        WHEN write_data =>            
          busy_prev <= i2c_busy;                       
          IF(busy_prev = '1' AND i2c_busy = '0') THEN  
            busy_cnt := busy_cnt + 1;                    
          END IF;
          CASE busy_cnt IS                            
            WHEN 0 =>                                    
              i2c_ena <= '1';                            
              i2c_addr <= "1001011";               
              i2c_rw <= '0';                              
              i2c_data_wr <= "10011001";                  
            WHEN 1 =>                                   
              i2c_data_wr <= "00110100";                  
            WHEN 2 =>                                  
              i2c_ena <= '0';                           
              IF(i2c_busy = '0') THEN                    
                busy_cnt := 0;                            
                state <= read_data;              
              END IF;
            WHEN OTHERS => NULL;
          END CASE;
          
        WHEN read_data =>
          busy_prev <= i2c_busy;                      
          IF(busy_prev = '1' AND i2c_busy = '0') THEN 
            busy_cnt := busy_cnt + 1;                  
          END IF;
          CASE busy_cnt IS                             
            WHEN 0 =>                                    
              i2c_ena <= '1';                             
              i2c_addr <= "1001011";               
              i2c_rw <= '1';                              
            WHEN 1 =>                                   
              IF(i2c_busy = '0') THEN                   
                temp_data(15 DOWNTO 8) <= i2c_data_rd;     
              END IF;
            WHEN 2 =>                                   
              i2c_ena <= '0';                             
              IF(i2c_busy = '0') THEN                    
                temp_data(7 DOWNTO 0) <= i2c_data_rd;      
                busy_cnt := 0;                            
                state <= start;                   
              END IF;
           WHEN OTHERS => NULL;
          END CASE;
          
        
        WHEN OTHERS =>
          state <= start;

      END CASE;
    END IF;
  END PROCESS;
    temperature <= temp_data;
   
end behave;