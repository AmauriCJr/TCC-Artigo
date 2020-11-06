----------------------------------------------------------------------------------
-- Company: Universidade de Brasília
-- Engineer: Amauri da Costa Júnior
-- 
-- Create Date:    17:28:58 11/02/2020 
-- Design Name: Recontrução da imagem do pé da minha mãe
-- Module Name:    TopLevel - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.numeric_std.all;
use ieee.math_real.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity TopLevel is
end TopLevel;

architecture Behavioral of TopLevel is



COMPONENT image1
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;

COMPONENT image4
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;

COMPONENT image2
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;

COMPONENT image3
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;



signal done1 : std_logic := '0';
signal wr_enable_re,wr_enable_im : STD_LOGIC_VECTOR(0 DOWNTO 0) := "0";
signal addr_rom_re,addr_rom_im,addr_ram_re,addr_ram_im : STD_LOGIC_VECTOR(5 DOWNTO 0) := (others => '0');
signal data_rom_re,data_in_ram_re,data_out_ram_re : STD_LOGIC_VECTOR(7 DOWNTO 0) := (others => '0');
signal data_rom_im,data_in_ram_im,data_out_ram_im : STD_LOGIC_VECTOR(7 DOWNTO 0) := (others => '0');


signal row_index,col_index : integer := 0;

signal VariavelTeste : STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '0');




  -----------------------------------------------------------------------
  -- Timing constants
  -----------------------------------------------------------------------
  constant CLOCK_PERIOD : time := 100 ns;
  constant T_HOLD       : time := 10 ns;
  constant T_STROBE     : time := CLOCK_PERIOD - (1 ns);
  
    signal	clk 				: STD_LOGIC;									--Clock
    signal	start 			: STD_LOGIC:= '1';							--Manda o processo de começar o carregamento de dados e fft começar
    signal	xn_re 			: STD_LOGIC_VECTOR(7 DOWNTO 0);			--Dados reais
    signal	xn_im 			: STD_LOGIC_VECTOR(7 DOWNTO 0);			--Dados imaginários
    signal	fwd_inv 			: STD_LOGIC:= '0';							--1 pra transformada direta, 0 pra inversa
    signal	fwd_inv_we 		: STD_LOGIC:= '1';							--permite a variavel de cima modificar
	 signal	scale_sch 		: STD_LOGIC_VECTOR(3 DOWNTO 0):= "0011";
    signal 	scale_sch_we 	: STD_LOGIC:= '1';
    signal	rfd 				: STD_LOGIC; 									--Daqui pra baixo é saida
    signal	xn_index 		: STD_LOGIC_VECTOR(2 DOWNTO 0); 
    signal	busy 				: STD_LOGIC;
    signal	edone 			: STD_LOGIC;
    signal	done 				: STD_LOGIC;
    signal	dv 				: STD_LOGIC;
    signal	xk_index 		: STD_LOGIC_VECTOR(2 DOWNTO 0);
    signal	xk_re 			: STD_LOGIC_VECTOR(7 DOWNTO 0);
    signal	xk_im 			: STD_LOGIC_VECTOR(7 DOWNTO 0);
	 
	 
	 signal	Nxk_re 			: STD_LOGIC_VECTOR(7 DOWNTO 0);
    signal	Nxk_im 			: STD_LOGIC_VECTOR(7 DOWNTO 0);
  
  


COMPONENT FFT
  PORT (
    clk : IN STD_LOGIC;
    start : IN STD_LOGIC;
    xn_re : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    xn_im : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    fwd_inv : IN STD_LOGIC;
    fwd_inv_we : IN STD_LOGIC;
    scale_sch : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    scale_sch_we : IN STD_LOGIC;
    rfd : OUT STD_LOGIC;
    xn_index : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
    busy : OUT STD_LOGIC;
    edone : OUT STD_LOGIC;
    done : OUT STD_LOGIC;
    dv : OUT STD_LOGIC;
    xk_index : OUT STD_LOGIC_VECTOR(2 DOWNTO 0);
    xk_re : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
    xk_im : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;


begin

FFT_IPCore : FFT
  PORT MAP (
    clk => clk,
    start => start,
    xn_re => xn_re,
    xn_im => xn_im,
    fwd_inv => fwd_inv,
    fwd_inv_we => fwd_inv_we,
    scale_sch => scale_sch,
    scale_sch_we => scale_sch_we,
    rfd => rfd,
    xn_index => xn_index,
    busy => busy,
    edone => edone,
    done => done,
    dv => dv,
    xk_index => xk_index,
    xk_re => xk_re,
    xk_im => xk_im
  );
  
  
  
  
  
  
  

  -----------------------------------------------------------------------
  -- Generate clock
  -----------------------------------------------------------------------

  clock_gen : process
  begin
    clk <= '0';
    wait for CLOCK_PERIOD/2;
    loop
      clk <= '0';
      wait for CLOCK_PERIOD/2;
      clk <= '1';
      wait for CLOCK_PERIOD/2;
    end loop;
  end process clock_gen;



image_rom_re : image1 port map(clk,addr_rom_re,data_rom_re);

image_rom_im : image4 port map(clk,addr_rom_im,data_rom_im);

image_ram_re : image2 port map(clk,wr_enable_re,addr_ram_re,data_in_ram_re,data_out_ram_re);

image_ram_im : image3 port map(clk,wr_enable_im,addr_ram_im,data_in_ram_im,data_out_ram_im);


-- Para a escrita das variáveis de configuração
process(clk)
begin
    if(rising_edge(clk)) then
		fwd_inv_we <= '0';
		scale_sch_we <= '0';
	 end if;
end process;


--REINICIA O PROCESSO DE LEITURA DA IMAGEM
--process(done)
--begin
--	if (done = '1') then
--	done1 <= '0';
--	end if;
--
--end process;




--LE A IMAGEM SEPARADA EM PARTE REAL E IMAGINARIA
process(clk)
begin
    if(rising_edge(clk)) then
        if(done1 = '0') then
			addr_rom_re <= addr_rom_re + "000001"; --start reading each pixel from rom
         addr_rom_im <= addr_rom_im + "000001";  
            --row and column index of the image.
            if(col_index = 7) then  --check if last column has reached. a matrix 3x4 tem 4 colunas que cabem em de 0 a 3
					col_index <= 0; --reset it to zero.
					--done1 <= '1';
                if(row_index = 7) then --check if last row has reached. a matrix 3x4 tem 3 linhas que cabem em de 0 a 2
						row_index <= 0; --reset it to zero
						done1 <= '1'; --the processing is done.
                else    
						row_index <= row_index + 1; --increment row index.
                end if;
            else
					col_index <= col_index + 1; --increment column index.
            end if; 
			end if;
     end if;       
end process; 


xn_re <= data_rom_re;
xn_im <= data_rom_im;




--wr_enable <= "1"; --write enable for the RAM
--data_in_ram_re <= xk_re;
--data_in_ram_im <= xk_im;
--addr_ram_re <= conv_std_logic_vector((col_index*3 + row_index),4);
----addr_ram_im <= conv_std_logic_vector((col_index*3 + row_index),4);
--        else
--        --this segment reads the transposed image(data written into RAM).
--wr_enable <= "0";  --after processing write enable is disabled
--addr_rom <= "000000"; 
--            if(addr_ram = "1011") then 
--addr_ram_re <= "000000";
--            else
--addr_ram_re <= addr_ram + 1;
--            end if; 
--        end if; 
--    end if;     
--end process;    
--
--end Behavioral;






end Behavioral;
