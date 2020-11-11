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
signal wr_enable: STD_LOGIC_VECTOR(0 DOWNTO 0) := "0";
signal addr_rom_re,addr_rom_im,addr_ram : STD_LOGIC_VECTOR(5 DOWNTO 0) := (others => '0');
signal data_rom_re,data_in_ram_re,data_out_ram_re : STD_LOGIC_VECTOR(7 DOWNTO 0) := (others => '0');
signal data_rom_im,data_in_ram_im,data_out_ram_im : STD_LOGIC_VECTOR(7 DOWNTO 0) := (others => '0');


signal row_index,col_index,finalizaT : integer := 0;

signal xk_index_row,xk_index_col : integer := -2;


signal contador : STD_LOGIC_VECTOR(3 DOWNTO 0) := (others => '0');

signal TransformadaPronta : STD_LOGIC := '0';

--signal VariavelTeste : STD_LOGIC_VECTOR(31 DOWNTO 0) := (others => '0');





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
	 --signal	xk_index_row	: STD_LOGIC_VECTOR(2 DOWNTO 0);			--Informação sobre a linha da matriz da transformada
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
  -- Gera o clock
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

image_ram_re : image2 port map(clk,wr_enable,addr_ram,data_in_ram_re,data_out_ram_re);

image_ram_im : image3 port map(clk,wr_enable,addr_ram,data_in_ram_im,data_out_ram_im);


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
			addr_rom_re <= addr_rom_re + "000001";
         addr_rom_im <= addr_rom_im + "000001";  
            if(col_index = 7) then
					col_index <= 0;
					--done1 <= '1';
                if(row_index = 7) then 
						row_index <= 0;
						done1 <= '1';
                else    
						row_index <= row_index + 1;
                end if;
            else
					col_index <= col_index + 1;
            end if;

----------------------------------------------------------------------------------------------






------------------------------------------------------------------------------------------------				
			end if;
     end if;       
end process; 


xn_re <= data_rom_re;
xn_im <= data_rom_im;




process(clk)   
begin
	if(rising_edge(clk)) and(edone = '1') then
		contador <= contador + "0001";
		if (contador = "1000") then
			TransformadaPronta <= '1';
		end if;
	end if;
end process;

process(clk)   
begin
	if(rising_edge(clk) and edone = '1') then
		xk_index_row <= xk_index_row + 1;
	end if;
end process;
      
xk_index_col <= conv_integer(xk_index);
		
process(clk)
begin
	if(rising_edge(clk)) then
		if(TransformadaPronta = '0') then
			wr_enable <= "1"; --liga o modo escrever
			data_in_ram_re <= xk_re;
			data_in_ram_im <= xk_im;
			addr_ram <= conv_std_logic_vector((xk_index_col*8 + xk_index_row),6); --o número 6 é o tamanho do vetor de endereço
		else
			if (finalizaT < 2) then
				wr_enable <= "0";  --desliga o modo escrever
				--addr_rom_re <= "000000"; 
				if(addr_ram = "111111") then 
					addr_ram <= "000000";
					finalizaT <= finalizaT +1;
            else
					addr_ram <= addr_ram + 1;
            end if; 
        end if; 
    end if; 
end if;    
end process;  

--process(clk)   
--begin
--	if(rising_edge(clk) and finalizaT = 2) then
--		
--	end if;
--end process;  



end Behavioral;

