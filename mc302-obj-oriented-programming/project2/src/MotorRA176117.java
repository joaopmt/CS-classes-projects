import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Um motor para o jogo LaMa - Lacaios e Magias.
 * 
 * @author João Pedro Martins
 *
 */
public class MotorRA176117 extends Motor {

	public MotorRA176117(Baralho deck1, Baralho deck2, ArrayList<Carta> mao1,
			ArrayList<Carta> mao2, Jogador jogador1, Jogador jogador2,
			int verbose, int tempoLimitado, PrintWriter saidaArquivo, EnumSet<Funcionalidade> funcionalidadesAtivas) {
		super(deck1, deck2, mao1, mao2, jogador1, jogador2, verbose,
				tempoLimitado, saidaArquivo, funcionalidadesAtivas);
		imprimir("========================");
		imprimir("*** Classe "+this.getClass().getName()+" inicializada.");
		imprimir("Funcionalidade ativas no Motor:");
		imprimir("INVESTIDA: "+(this.funcionalidadesAtivas.contains(Funcionalidade.INVESTIDA)?"SIM":"NAO"));
		imprimir("ATAQUE_DUPLO: "+(this.funcionalidadesAtivas.contains(Funcionalidade.ATAQUE_DUPLO)?"SIM":"NAO"));
		imprimir("PROVOCAR: "+(this.funcionalidadesAtivas.contains(Funcionalidade.PROVOCAR)?"SIM":"NAO"));
		imprimir("========================");
	}
	
	private int jogador; // 1 == turno do jogador1, 2 == turno do jogador2.
	private int turno;
	private int nCartasHeroi1;
	private int nCartasHeroi2;
	
	private Mesa mesa;
	
	// "Apontadores" - Assim podemos programar genericamente os métodos para funcionar com ambos os jogadores
	private ArrayList<Carta> mao;
	private ArrayList<Carta> lacaios;
	private ArrayList<Carta> lacaiosOponente;
	
	private int manaDisponivel;
	
	// "Memória" - Para marcar ações que só podem ser realizadas uma vez por turno.
	private boolean poderHeroicoUsado;
	private HashSet<Integer> lacaiosAtacaramID;

	/**
	 * Método principal da classe MotorRa176117,
	 * responsável por organizar uma partida de
	 * LaMa e utilizar sub-métodos para processar
	 * e imprimir todas as informações pertinentes.
	 * 
	 */
	@Override
	public int executarPartida() throws LamaException {
		vidaHeroi1 = vidaHeroi2 = 30;
		manaJogador1 = manaJogador2 = 1;
		nCartasHeroi1 = cartasIniJogador1; 
		nCartasHeroi2 = cartasIniJogador2;
		ArrayList<Jogada> movimentos = new ArrayList<Jogada>();
		int noCardDmgCounter1 = 1;
		int noCardDmgCounter2 = 1;
		turno = 1;
		
		// São 60 turnos
		for(int k = 0; k < 60; k++){
			// Atualiza a mana disponível para cada jogador
			manaJogador1 = manaJogador2 = turno >= 10? 10: turno;
			if(turno == 1) manaJogador2++;
			
			imprimir("\n=== TURNO "+turno+" ===\n");
			
			// Atualiza mesa (com cópia profunda)
			@SuppressWarnings("unchecked")
			ArrayList<CartaLacaio> lacaios1clone = (ArrayList<CartaLacaio>) UnoptimizedDeepCopy.copy(lacaiosMesa1);
			@SuppressWarnings("unchecked")
			ArrayList<CartaLacaio> lacaios2clone = (ArrayList<CartaLacaio>) UnoptimizedDeepCopy.copy(lacaiosMesa2);
			mesa = new Mesa(lacaios1clone, lacaios2clone, vidaHeroi1, vidaHeroi2, nCartasHeroi1+1, nCartasHeroi2, turno>10?10:turno, turno>10?10:(turno==1?2:turno));
						
			// Apontadores para jogador1
			mao = maoJogador1;
			lacaios = lacaiosMesa1;
			lacaiosOponente = lacaiosMesa2;
			jogador = 1;
			
			// Processa um turno do Jogador1
			imprimir("\n----------------------- Começo de turno para Jogador 1:");
			long startTime, endTime, totalTime;
			
			// Cópia profunda de jogadas realizadas.
			@SuppressWarnings("unchecked")
			ArrayList<Jogada> cloneMovimentos1 = (ArrayList<Jogada>) UnoptimizedDeepCopy.copy(movimentos);
			
			// Obtém as jogadas do jogador 1
			startTime = System.nanoTime();
			if(baralho1.getCartas().size() > 0){
				if(nCartasHeroi1 >= maxCartasMao){
					movimentos = jogador1.processarTurno(mesa, null, cloneMovimentos1);
					comprarCarta(); // carta descartada
				}
				else
					movimentos = jogador1.processarTurno(mesa, comprarCarta(), cloneMovimentos1);
			}
			else{
				imprimir("Fadiga: O Herói 1 recebeu "+noCardDmgCounter1+" de dano por falta de cartas no baralho. (Vida restante: "+(vidaHeroi1-noCardDmgCounter1)+").");
				vidaHeroi1 -= noCardDmgCounter1++;
				if( vidaHeroi1 <= 0){
					// Jogador 2 venceu
					imprimir("O jogador 2 venceu porque o jogador 1 recebeu um dano mortal por falta de cartas ! (Dano : " +(noCardDmgCounter1-1)+ ", Vida Herói 1: "+vidaHeroi1+")");
					return 2;
				}
				movimentos = jogador1.processarTurno(mesa, null, cloneMovimentos1);
			}
			endTime = System.nanoTime();
			totalTime = endTime - startTime;
			if(tempoLimitado!=0 && totalTime > 3e8){ // 300ms
				// Limite de tempo excedido pelo jogador 1. Vitoria do jogador 2.
				return 2;
			}
			else
				imprimir("Tempo usado em processarTurno(): "+totalTime/1e6+"ms");

			// Processa as jogadas do jogador 1
			this.poderHeroicoUsado = false;
            this.lacaiosAtacaramID = new HashSet<Integer>();
            this.manaDisponivel = manaJogador1;
			for(int i = 0; i < movimentos.size(); i++){
				processarJogada(movimentos.get(i));
			}
			if(vidaHeroi2 <= 0){
				// Vitoria do jogador 1
				return 1;
			}
			
			// Atualiza mesa (com cópia profunda)
			@SuppressWarnings("unchecked")
			ArrayList<CartaLacaio> lacaios1clone2 = (ArrayList<CartaLacaio>) UnoptimizedDeepCopy.copy(lacaiosMesa1);
			@SuppressWarnings("unchecked")
			ArrayList<CartaLacaio> lacaios2clone2 = (ArrayList<CartaLacaio>) UnoptimizedDeepCopy.copy(lacaiosMesa2);
			mesa = new Mesa(lacaios1clone2, lacaios2clone2, vidaHeroi1, vidaHeroi2, nCartasHeroi1, nCartasHeroi2+1, turno>10?10:turno, turno>10?10:(turno==1?2:turno));
						
			// Apontadores para jogador2
			mao = maoJogador2;
			lacaios = lacaiosMesa2;
			lacaiosOponente = lacaiosMesa1;
			jogador = 2;
			
			// Processa um turno do Jogador2
			imprimir("\n\n----------------------- Começo de turno para Jogador 2:");
			
			// Cópia profunda de jogadas realizadas.
			@SuppressWarnings("unchecked")
			ArrayList<Jogada> cloneMovimentos2 = (ArrayList<Jogada>) UnoptimizedDeepCopy.copy(movimentos);
			
			startTime = System.nanoTime();

			// Obtém as jogadas do jogador 2: 
			
			if(baralho2.getCartas().size() > 0){
				if(nCartasHeroi2 >= maxCartasMao){
					movimentos = jogador2.processarTurno(mesa, null, cloneMovimentos2);
					comprarCarta(); // carta descartada
				}
				else
					movimentos = jogador2.processarTurno(mesa, comprarCarta(), cloneMovimentos2);
			}
			else{
				imprimir("Fadiga: O Herói 2 recebeu "+noCardDmgCounter2+" de dano por falta de cartas no baralho. (Vida restante: "+(vidaHeroi2-noCardDmgCounter2)+").");
				vidaHeroi2 -= noCardDmgCounter2++;
				if( vidaHeroi2 <= 0){
					// Vitoria do jogador 1
					imprimir("O jogador 1 venceu porque o jogador 2 recebeu um dano mortal por falta de cartas ! (Dano : " +(noCardDmgCounter2-1)+ ", Vida Herói 2: "+vidaHeroi2 +")");
					return 1;
				}
				movimentos = jogador2.processarTurno(mesa, null, cloneMovimentos2);
			}
			
			endTime = System.nanoTime();
			totalTime = endTime - startTime;
			if(tempoLimitado!=0 && totalTime > 3e8){ // 300ms
				// Limite de tempo excedido pelo jogador 2. Vitoria do jogador 1.
				return 1;
			}
			else
				imprimir("Tempo usado em processarTurno(): "+totalTime/1e6+"ms");
			
			// Processa as jogadas do jogador 2
			this.poderHeroicoUsado = false;
            this.lacaiosAtacaramID = new HashSet<Integer>();
            this.manaDisponivel = manaJogador2;
			for(int i = 0; i < movimentos.size(); i++){
				processarJogada(movimentos.get(i));
			}
			if(vidaHeroi1 <= 0){
				// Vitoria do jogador 2
				return 2;
			}
			
			// Passa para o próximo turno
			turno++;
		}
		
		// Nunca vai chegar aqui dependendo do número de rodadas
		imprimir("Erro: A partida não pode ser determinada em mais de 60 rounds. Provavel BUG.");
		throw new LamaException(-1, null, "Erro desconhecido. Mais de 60 turnos sem termino do jogo.", 0);
	}

	/**
	 * Processa a jogada passada como parâmetro, i.e., verifica e trata
	 * possíveis erros cometidos pelo jogador na jogada e atualiza
	 * o estado de todos os objetos pertinentes envolvidos após uma
	 * jogada válida.
	 */
	protected void processarJogada(Jogada umaJogada) throws LamaException {
		String erroMensagem;
		
		List<Carta> todasAsCartasDoJogo = obterTodasAsCartas();
		Carta cartaJogada = obterCartaOriginal(umaJogada.getCartaJogada(), todasAsCartasDoJogo);
		Carta alvo = obterCartaOriginal(umaJogada.getCartaAlvo(), todasAsCartasDoJogo);
		
		switch(umaJogada.getTipo()){
		case ATAQUE:	
			
			imprimir("JOGADA: Um ataque do lacaio "
					+(cartaJogada==null?"[null]":(cartaJogada.getNome()+"(ID="+cartaJogada.getID()))+") no "
					+(alvo==null?("Herói "+(jogador==1?2:1)):("lacaio "+alvo.getNome()+"(ID="+alvo.getID()+")"))+'.');
			
			// Trata uma possível jogada inválida:
			
			// Erro 5 (Atacar com um lacaio invalido de origem do ataque)
			if(!lacaios.contains(cartaJogada)){
				erroMensagem = "Erro: Tentou-se atacar com uma referência nula de Carta, "
							 + "ou com um lacaio que não está na mesa ou não pertence a este Jogador.\n"
							 + "Origem do ataque: carta_id="+(cartaJogada==null?"[null]":cartaJogada.getID())+".";
				imprimir(erroMensagem);
				throw new LamaException(5, umaJogada, erroMensagem, jogador==1?2:1);
			}
			// Erro 6 (Atacar com um lacaio que foi baixado neste turno)
			if(((CartaLacaio)cartaJogada).getTurno() == turno
			&& !(funcionalidadesAtivas.contains(Funcionalidade.INVESTIDA)
			&& ((CartaLacaio)cartaJogada).getEfeito() == TipoEfeito.INVESTIDA)){
				erroMensagem = "Erro: Tentou-se atacar com um lacaio que foi baixado neste turno"
							 + (funcionalidadesAtivas.contains(Funcionalidade.INVESTIDA)?
							  " e não possúi o efeito Investida.":'.')
						 	 + "\nOrigem do ataque: carta_id="+cartaJogada.getID()+".";
				imprimir(erroMensagem);
				throw new LamaException(6, umaJogada, erroMensagem, jogador==1?2:1);	
			}
			// Erro 7 (Atacar com um lacaio mais de uma vez por turno)
			if(lacaiosAtacaramID.contains(cartaJogada.getID())){
				erroMensagem = "Erro: Tentou-se atacar com um lacaio que já atacou neste turno"
							 + " e não possúi o efeito Ataque Duplo."
							 + "\nOrigem do ataque: carta_id="+cartaJogada.getID()+".";
				imprimir(erroMensagem);
				throw new LamaException(7, umaJogada, erroMensagem, jogador==1?2:1);
			}
			// Erro 8 (Atacar com um lacaio um alvo inválido)
			if(alvo != null && !lacaiosOponente.contains(alvo)){
				erroMensagem = "Erro: Tentou-se atacar um Lacaio que não é do oponente ou não existe na mesa."
						 	 + "\nOrigem do ataque: carta_id="+cartaJogada.getID()+".\nAlvo: carta_id="+alvo.getID()
						 	 +".IDs de Lacaios do oponente: ";
				for(Carta card : lacaiosOponente){
					erroMensagem += card.getID() + "  ";
				}
				imprimir(erroMensagem);
				throw new LamaException(8, umaJogada, erroMensagem, jogador==1?2:1);
			}
			// Erro 13 (Existindo um lacaio com provocar, atacar outro alvo)
			if(funcionalidadesAtivas.contains(Funcionalidade.PROVOCAR)){
				List<Carta> lacaiosHabProvocar = lacaiosOponente.stream()
						.filter(card->((CartaLacaio)card).getEfeito() == TipoEfeito.PROVOCAR)
						.collect(Collectors.toList());
				if(!lacaiosHabProvocar.isEmpty() && (alvo == null || ((CartaLacaio)alvo).getEfeito() != TipoEfeito.PROVOCAR)){
					erroMensagem = "Erro: O oponente possui lacaio(s) com o efeito PROVOCAR "
								 + "e o alvo do ataque não é um lacaio com esse efeito."
								 + "\nOrigem do ataque: carta_id="+cartaJogada.getID()
								 + ".\nAlvo: "+(alvo==null?("Herói "+(jogador==1?2:1)):"carta_id="+alvo.getID())
								 + ".\nIDs de Lacaios do oponente com efeito PROVOCAR: ";
					for(Carta card : lacaiosHabProvocar){
						erroMensagem += card.getID() + "  ";
					}
					imprimir(erroMensagem);
					throw new LamaException(13, umaJogada, erroMensagem, jogador==1?2:1);
				}
			}
			
			// Processa uma jogada válida:
			
			CartaLacaio lacAtacou = (CartaLacaio) cartaJogada;
			if(alvo != null){
				CartaLacaio lacAlvo = (CartaLacaio) alvo;
				
				// Calcula os danos do ataque e ajusta as vidas dos lacaios
				lacAlvo.setVidaAtual(lacAlvo.getVidaAtual()-lacAtacou.getAtaque());
				lacAtacou.setVidaAtual(lacAtacou.getVidaAtual()-lacAlvo.getAtaque());
				
				if(lacAlvo.getVidaAtual() > 0)
					imprimir("O lacaio "+lacAlvo.getNome()+"(ID="+lacAlvo.getID()+") (alvo) sofreu "
							 +lacAtacou.getAtaque()+" de dano (vida restante: "+lacAlvo.getVidaAtual()+").");
				else{
					imprimir("O lacaio "+lacAlvo.getNome()+"(ID="+lacAlvo.getID()+") (alvo) sofreu "
							 +lacAtacou.getAtaque()+" de dano e morreu.");
					lacaiosOponente.remove(lacAlvo);
				}
				if(lacAtacou.getVidaAtual() > 0)
					imprimir("O lacaio "+lacAtacou.getNome()+"(ID="+lacAtacou.getID()+") (que atacou) sofreu "
							 +lacAlvo.getAtaque()+" de dano (vida restante: "+lacAtacou.getVidaAtual()+").");
				else{
					imprimir("O lacaio "+lacAtacou.getNome()+"(ID="+lacAtacou.getID()+") (que atacou) sofreu "
							 +lacAlvo.getAtaque()+" de dano e morreu.");
					lacaios.remove(lacAtacou);
				}
			// Se o alvo for o herói do oponente
			} else{
				switch(jogador){
				case 1:
					vidaHeroi2 -= lacAtacou.getAtaque();
					imprimir("O Héroi 2 sofreu "+lacAtacou.getAtaque()+" de dano (vida restante: "+vidaHeroi2+").");
					break;
				case 2:
					vidaHeroi1 -= lacAtacou.getAtaque();
					imprimir("O Héroi 1 sofreu "+lacAtacou.getAtaque()+" de dano (vida restante: "+vidaHeroi1+").");
					break;
				}
			}
			
			// Garante um ataque extra caso o lacaio tenha Ataque Duplo
			if(funcionalidadesAtivas.contains(Funcionalidade.ATAQUE_DUPLO)
			&& lacAtacou.getEfeito() == TipoEfeito.ATAQUE_DUPLO){
				lacAtacou.setEfeito(TipoEfeito.NADA); // remove o efeito
			}
			else lacaiosAtacaramID.add(lacAtacou.getID());
			break;
			
		case LACAIO:
			
			imprimir("JOGADA: O lacaio "+(cartaJogada==null?"[null]":cartaJogada.getNome()
					+"(ID="+cartaJogada.getID()+")")+ " foi baixado para a mesa.");
			
			// Trata uma possível jogada inválida:
			
			// Erro 1 (Baixar lacaio ou usar magia sem possuir a carta na mão)
			if(!mao.contains(cartaJogada)){
				erroMensagem = "Erro: Tentou-se usar uma carta que não existe na mão:"
							 + "carta_id="+(cartaJogada==null?"[null]":cartaJogada.getID())+"."
							 + "IDs de cartas na mao: ";
				for(Carta card : mao){
					erroMensagem += card.getID() + ", ";
				}
				imprimir(erroMensagem);
				throw new LamaException(1, umaJogada, erroMensagem, jogador==1?2:1);
			}
			// Erro 2 (Realizar uma jogada que o limite de mana não permite)
			if(manaDisponivel < cartaJogada.getMana()){
				erroMensagem = "Erro: Mana insuficiente para realizar a jogada."
						 	 + "\nTipo da jogada: "+umaJogada.getTipo()
						 	 + ".\nCusto (em mana) da Jogada: "+cartaJogada.getMana()
						 	 + ".\nMana disponível: "+manaDisponivel+'.';
				imprimir(erroMensagem);
				throw new LamaException(2, umaJogada, erroMensagem, jogador==1?2:1);
			}
			// Erro 3 (Tentar baixar uma carta de magia como carta lacaio)
			if(cartaJogada instanceof CartaMagia){
				erroMensagem = "Erro: Tentativa de baixar a magia "
							 + cartaJogada.getNome()+"(ID="+cartaJogada.getID()+")"
							 + " como um lacaio.";
				imprimir(erroMensagem);
				throw new LamaException(3, umaJogada, erroMensagem, jogador==1?2:1);
			}
			//Erro 4 (Baixar um lacaio já tendo sete outros lacaios em mesa)
			if(lacaios.size() >= 7){
				erroMensagem = "Erro: Tentativa de baixar o lacaio "
							 + cartaJogada.getNome()+"(ID="+cartaJogada.getID()+") "
							 + "sendo que já há 7 lacaios na mesa.";
				imprimir(erroMensagem);
				throw new LamaException(4, umaJogada, erroMensagem, jogador==1?2:1);
			}
			
			// Processa uma jogada válida:
			
			CartaLacaio lacaioBaixado = (CartaLacaio) cartaJogada;
			lacaioBaixado.setTurno(turno); // seta o turno
			lacaios.add(lacaioBaixado); // lacaio adicionado à mesa
			mao.remove(lacaioBaixado); // lacaio retirado da mão
			manaDisponivel -= lacaioBaixado.getMana(); // mana atualizada
			break;
			
		case MAGIA:
			
			// Trata uma possível jogada inválida (erros gerais de magia):
			
			// Erro 1 (Baixar lacaio ou usar magia sem possuir a carta na mão)
			if(!mao.contains(cartaJogada)){
				erroMensagem = "Erro: Tentou-se usar uma carta que não existe na mão:"
							 + "carta_id="+(cartaJogada==null?"[null]":cartaJogada.getID())+"."
							 + "IDs de cartas na mao: ";
				for(Carta card : mao){
					erroMensagem += card.getID() + ", ";
				}
				imprimir(erroMensagem);
				throw new LamaException(1, umaJogada, erroMensagem, jogador==1?2:1);
			}
			// Erro 2 (Realizar uma jogada que o limite de mana não permite)
			if(manaDisponivel < cartaJogada.getMana()){
				erroMensagem = "Erro: Mana insuficiente para realizar a jogada."
						 	+ "\nTipo da jogada: "+umaJogada.getTipo()
						 	+ ".\nCusto (em mana) da Jogada: "+cartaJogada.getMana()
						 	+ ".\nMana disponível: "+manaDisponivel+'.';
				imprimir(erroMensagem);
				throw new LamaException(2, umaJogada, erroMensagem, jogador==1?2:1);
			}
			// Erro 9 (Tentar usar uma carta de lacaio como uma magia)
			if(cartaJogada instanceof CartaLacaio){
				erroMensagem = "Erro: Tentativa de usar o lacaio "
							 + cartaJogada.getNome()+"(ID="+cartaJogada.getID()+")"
							 + " como uma magia.";
				imprimir(erroMensagem);
				throw new LamaException(9, umaJogada, erroMensagem, jogador==1?2:1);
			}
			
			CartaMagia magiaUtilizada = (CartaMagia) cartaJogada;
			
			switch(magiaUtilizada.getMagiaTipo()){
			case ALVO:
				
				imprimir("JOGADA: A magia de alvo "+magiaUtilizada.getNome()+"(ID="+magiaUtilizada.getID()+") foi utilizada no "
				+(alvo==null?("Herói "+(jogador==1?2:1)):("lacaio "+alvo.getNome()+"(ID="+alvo.getID()+")"))+'.');
				
				// Trata uma possível jogada inválida (erros específicos de magia de alvo):
				
				// Erro 10 (Usar uma magia de alvo ou buff em um alvo inválido)
				if(alvo != null && !lacaiosOponente.contains(alvo)){
					erroMensagem = "Erro: Tentou-se usar magia de alvo em um lacaio que não é do oponente ou não existe na mesa."
						 	 	 + "\nOrigem do ataque: carta_id="+cartaJogada.getID()+".\nAlvo: carta_id="+alvo.getID()
						 	 	 + ".\nIDs de Lacaios do oponente: ";
					for(Carta card : lacaiosOponente){
						erroMensagem += card.getID() + "  ";
					}
					imprimir(erroMensagem);
					throw new LamaException(10, umaJogada, erroMensagem, jogador==1?2:1);
				}
				
				// Processa uma jogada válida:
				
				if(alvo != null){
					CartaLacaio lacAlvo = (CartaLacaio) alvo;
					
					// Calcula o dano sofrido pelo alvo
					lacAlvo.setVidaAtual(lacAlvo.getVidaAtual()-magiaUtilizada.getMagiaDano());
					
					if(lacAlvo.getVidaAtual() > 0)
						imprimir("O lacaio "+lacAlvo.getNome()+"(ID="+lacAlvo.getID()+") (alvo) sofreu "
								 +magiaUtilizada.getMagiaDano()+" de dano (vida restante: "+lacAlvo.getVidaAtual()+").");
					else{
						imprimir("O lacaio "+lacAlvo.getNome()+"(ID="+lacAlvo.getID()+") (alvo) sofreu "
								 +magiaUtilizada.getMagiaDano()+" de dano e morreu.");
						lacaiosOponente.remove(lacAlvo);
					}
				// Se o alvo for o herói do oponente
				} else{
					switch(jogador){
					case 1:
						vidaHeroi2 -= magiaUtilizada.getMagiaDano();
						imprimir("O Héroi 2 sofreu "+magiaUtilizada.getMagiaDano()+"de dano (vida restante: "+vidaHeroi2+").");
						break;
					case 2:
						vidaHeroi1 -= magiaUtilizada.getMagiaDano();
						imprimir("O Héroi 1 sofreu "+magiaUtilizada.getMagiaDano()+"de dano (vida restante: "+vidaHeroi1+").");
						break;
					}
				}
				
				break;
			case AREA:
				
				imprimir("JOGADA: A magia de área "+magiaUtilizada.getNome()+"(ID="+magiaUtilizada.getID()+") foi utilizada");
				
				//Processa uma jogada válida:
				
				// Calcula o dano sofrido pelos alvos
				for(int i = lacaiosOponente.size()-1; i >= 0; i--){
					CartaLacaio lacAlvo = (CartaLacaio) lacaiosOponente.get(i);
					
					lacAlvo.setVidaAtual(lacAlvo.getVidaAtual()-magiaUtilizada.getMagiaDano());
					
					if(lacAlvo.getVidaAtual() > 0)
						imprimir("O lacaio "+lacAlvo.getNome()+"(ID="+lacAlvo.getID()+") (alvo) sofreu "
								 +magiaUtilizada.getMagiaDano()+" de dano (vida restante: "+lacAlvo.getVidaAtual()+").");
					else{
						imprimir("O lacaio "+lacAlvo.getNome()+"(ID="+lacAlvo.getID()+") (alvo) sofreu "
								 +magiaUtilizada.getMagiaDano()+" de dano e morreu.");
						lacaiosOponente.remove(lacAlvo);
					}
				}
				// Calcula o dano sofrido pelo Herói do oponente
				switch(jogador){
				case 1:
					vidaHeroi2 -= magiaUtilizada.getMagiaDano();
					imprimir("O Héroi 2 sofreu "+magiaUtilizada.getMagiaDano()+"de dano (vida restante: "+vidaHeroi2+").");
					break;
				case 2:
					vidaHeroi1 -= magiaUtilizada.getMagiaDano();
					imprimir("O Héroi 1 sofreu "+magiaUtilizada.getMagiaDano()+"de dano (vida restante: "+vidaHeroi1+").");
					break;
				}
				
				break;
			case BUFF:
				
				imprimir("JOGADA: A magia de buff "+magiaUtilizada.getNome()+"(ID="+magiaUtilizada.getID()+") foi utilizada"
					   +(alvo==null?(" em null"):(" no lacaio "+alvo.getNome()+"(ID="+alvo.getID()+")"))+'.');
				
				// Trata uma possível jogada inválida (erros específicos de magia de buff):
				
				// Erro 10 (Usar uma magia de alvo ou buff em um alvo inválido)
				if(alvo == null){
					erroMensagem = "Erro: Tentou-se usar magia de buff em uma referência nula de Carta."
					 	 	 	 + "\nOrigem do ataque: carta_id="+cartaJogada.getID()+".\nAlvo: [null]";
					imprimir(erroMensagem);
					throw new LamaException(10, umaJogada, erroMensagem, jogador==1?2:1);
				}
				if(!lacaios.contains(alvo)){
					erroMensagem = "Erro: Tentou-se usar magia de buff em um lacaio que é do oponente ou que não existe na mesa."
					 	 	 + "\nOrigem do ataque: carta_id="+cartaJogada.getID()+".\nAlvo: carta_id="+alvo.getID()
					 	 	 + ".IDs de lacaios deste jogador: ";
					for(Carta card : lacaios){
						erroMensagem += card.getID() + "  ";
					}
					imprimir(erroMensagem);
					throw new LamaException(10, umaJogada, erroMensagem, jogador==1?2:1);
				}
				
				// Processa uma jogada válida:
				
				CartaLacaio lacAlvo = (CartaLacaio) alvo;
				
				// Buffa o alvo
				lacAlvo.setAtaque(lacAlvo.getAtaque()+magiaUtilizada.getMagiaDano());
				lacAlvo.setVidaAtual(lacAlvo.getVidaAtual()+magiaUtilizada.getMagiaDano());
				lacAlvo.setVidaMaxima(lacAlvo.getVidaAtual()>lacAlvo.getVidaMaxima()?lacAlvo.getVidaAtual():lacAlvo.getVidaMaxima());
				
				imprimir("O lacaio "+lacAlvo.getNome()+"(ID="+lacAlvo.getID()+") recebeu um buff."
					   + "\nAtaque antigo: "+(lacAlvo.getAtaque()-magiaUtilizada.getMagiaDano())
					   + "\nAtaque novo: "+lacAlvo.getAtaque()
					   + "\nVida antiga: "+(lacAlvo.getVidaAtual()-magiaUtilizada.getMagiaDano())
					   + "\nVida nova: "+lacAlvo.getVidaAtual());
				
				break;
			default: break;
			}
			
			manaDisponivel -= cartaJogada.getMana(); // mana atualizada
			break;
			
		case PODER:
			
			imprimir("JOGADA: Utilizado Poder Heróico do Herói "+(jogador==1?1:2)+" no "
			+(alvo==null?("Herói "+(jogador==1?2:1)):("lacaio "+alvo.getNome()+"(ID="+alvo.getID()+")"))+'.');
	
			// Trata uma possível jogada inválida:
			
			// Erro 2 (Realizar uma jogada que o limite de mana não permite)
			if(manaDisponivel < 2){
				erroMensagem = "Erro: Mana insuficiente para realizar a jogada."
						 	+ "\nTipo da jogada: "+umaJogada.getTipo()
						 	+ ".\nCusto (em mana) da Jogada: 2"
						 	+ ".\nMana disponível: "+manaDisponivel+'.';
				imprimir(erroMensagem);
				throw new LamaException(2, umaJogada, erroMensagem, jogador==1?2:1);
			}
			// Erro 11 (Usar o poder heróico mais de uma vez por turno)
			if(poderHeroicoUsado){
				erroMensagem = "Erro: Tentativa de usar o Poder Heróico mais de uma vez por turno.\n"
							 + "Alvo: "+(alvo==null?("Herói "+(jogador==1?2:1)):("lacaio "+alvo.getNome()+"(ID="+alvo.getID()+")"))+'.';
				imprimir(erroMensagem);
				throw new LamaException(11, umaJogada, erroMensagem, jogador==1?2:1);
			}
			// Erro 12 (Usar o poder heróico em um alvo inválido)
			if(alvo != null && !(lacaiosOponente.contains(alvo))){
				erroMensagem = "Erro: Tentativa de usar o Poder Heróico em um alvo inválido."
						 	 + "\nAlvo: "+alvo.getNome()+"(ID="+alvo.getID()+")."
							 + ".\nIDs de Lacaios do oponente: ";
				for(Carta card : lacaiosOponente){
					erroMensagem += card.getID() + "  ";
				}
				imprimir(erroMensagem);
				throw new LamaException(12, umaJogada, erroMensagem, jogador==1?2:1);
			}
			// Erro 13 (Existindo um lacaio com provocar, atacar outro alvo)
			if(funcionalidadesAtivas.contains(Funcionalidade.PROVOCAR)){
				List<Carta> lacaiosHabProvocar = lacaiosOponente.stream()
						.filter(card->((CartaLacaio)card).getEfeito() == TipoEfeito.PROVOCAR)
						.collect(Collectors.toList());
				if(!lacaiosHabProvocar.isEmpty() && (alvo == null || ((CartaLacaio)alvo).getEfeito() != TipoEfeito.PROVOCAR)){
					erroMensagem = "Erro: O oponente possui lacaio(s) com o efeito PROVOCAR "
								 + "e o alvo do Poder Heróico não é um lacaio com esse efeito."
								 + "\nAlvo: "+(alvo==null?("Herói "+(jogador==1?2:1)):"carta_id="+alvo.getID())
								 + ".\nIDs de Lacaios do oponente com efeito PROVOCAR: ";
					for(Carta card : lacaiosHabProvocar){
						erroMensagem += card.getID() + "  ";
					}
					imprimir(erroMensagem);
					throw new LamaException(13, umaJogada, erroMensagem, jogador==1?2:1);
				}
			}
						
			// Processa uma jogada válida:
			
			if(alvo != null){
				CartaLacaio lacAlvo = (CartaLacaio) alvo;
				
				lacAlvo.setVidaAtual(lacAlvo.getVidaAtual()-1);
				
				if(lacAlvo.getVidaAtual() > 0)
					imprimir("O lacaio "+lacAlvo.getNome()+"(ID="+lacAlvo.getID()+") (alvo) sofreu "
							 +1+" de dano (vida restante: "+lacAlvo.getVidaAtual()+").");
				else{
					imprimir("O lacaio "+lacAlvo.getNome()+"(ID="+lacAlvo.getID()+") (alvo) sofreu "
							 +1+" de dano e morreu.");
					lacaiosOponente.remove(lacAlvo);
				}
				switch(jogador){
				case 1:
					vidaHeroi1 -= lacAlvo.getAtaque();
					imprimir("O Héroi 1 sofreu "+lacAlvo.getAtaque()+" de dano (vida restante: "+vidaHeroi1+").");
					break;
				case 2:
					vidaHeroi2 -= lacAlvo.getAtaque();
					imprimir("O Héroi 2 sofreu "+lacAlvo.getAtaque()+" de dano (vida restante: "+vidaHeroi2+").");
					break;
				}
			// Se o alvo for o herói do oponente
			} else{
				switch(jogador){
				case 1:
					vidaHeroi2 -= 1;
					imprimir("O Héroi 2 sofreu "+1+"de dano (vida restante: "+vidaHeroi2+").");
					break;
				case 2:
					vidaHeroi1 -= 1;
					imprimir("O Héroi 1 sofreu "+1+"de dano (vida restante: "+vidaHeroi1+").");
					break;
				}
			}
			
			poderHeroicoUsado = true; // o poder foi usado nesse turno
			manaDisponivel -= 2; // mana atualizada
			break;
			
		default: break;
			
		}
		return;
	}
	
	private Carta comprarCarta(){
		if(this.jogador == 1){
			if(baralho1.getCartas().size() <= 0)
				throw new RuntimeException("Erro: Não há mais cartas no baralho para serem compradas.");
			Carta nova = baralho1.comprarCarta();
			mao.add(nova);
			nCartasHeroi1++;
			return (Carta) UnoptimizedDeepCopy.copy(nova);
		}
		else{
			if(baralho2.getCartas().size() <= 0)
				throw new RuntimeException("Erro: Não há mais cartas no baralho para serem compradas.");
			Carta nova = baralho2.comprarCarta();
			mao.add(nova);
			nCartasHeroi2++;
			return (Carta) UnoptimizedDeepCopy.copy(nova);
		}
	}

	/**
	 * Obtém todas as cartas que estão na mesa, nas mãos dos jogadores,
	 * ou ainda nos decks dos jogadores. Ou seja, obtém todas as cartas
	 * do jogo.
	 * 
	 * @return uma ArrayList contendo todas as cartas
	 */
	private List<Carta> obterTodasAsCartas(){
		List<Carta> list = new ArrayList<Carta>();
		
		for(Carta card : lacaiosMesa1) list.add(card); 
		for(Carta card : lacaiosMesa2) list.add(card);
		for(Carta card : maoJogador1) list.add(card);
		for(Carta card : maoJogador2) list.add(card);
		for(Carta card : baralho1.getCartas()) list.add(card);
		for(Carta card : baralho2.getCartas()) list.add(card);
		
		return list;
	}
	
	/**
	 * Procura a carta original (que está em uma das listas de cartas
	 * pertencentes ao motor) da carta cópia passada como parâmetro.
	 * 
	 * Esse método é utilizado porque as cartas obtidas dos objetos Jogada
	 * (retornadas pelos métodos Jogada.getCartaJogada() e Jogada.getCartaAlvo())
	 * são cópias das cartas originais pertencentes ao motor que podem ter
	 * sido modificadas pelos jogadores, possivelmente estando com os valores
	 * dos atributos errados.
	 * 
	 * @param card
	 * @param list
	 * @return a carta original com valores corretos (caso encontre)
	 * ou null (caso não encontre)
	 */
	private Carta obterCartaOriginal(Carta card, List<Carta> list){
		
		if(card == null) return null;
		
		for(Carta card2 : list) if(card2.equals(card)) return card2;
		
		// Se não encontrar a carta retorna ela própria
		return card;
	}
}
