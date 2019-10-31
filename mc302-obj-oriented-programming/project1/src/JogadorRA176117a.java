import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

/**
* Esta classe representa um jogador melhorado em relação ao definido no Lab 5.
* 
* @see java.lang.Object
* @author João Pedro Martins
*/
public class JogadorRA176117a extends Jogador {
	private ArrayList<CartaLacaio> mesaLacaios;
	private ArrayList<CartaLacaio> mesaLacaiosOponente;
	private enum Modo {AGRESSION, CONTROL};
	Modo modo;
	
	/**
	  * O método construtor do JogadorRA176117.
	  * 
	  * @param maoInicial Contém a mão inicial do jogador. Deve conter o número de cartas correto dependendo se esta classe Jogador que está sendo construída é o primeiro ou o segundo jogador da partida. 
	  * @param primeiro   Informa se esta classe Jogador que está sendo construída é o primeiro jogador a iniciar nesta jogada (true) ou se é o segundo jogador (false).
	  */
	public JogadorRA176117a(ArrayList<Carta> maoInicial, boolean primeiro){
		primeiroJogador = primeiro;
		
		mao = maoInicial;
		mesaLacaios = new ArrayList<CartaLacaio>();
		mesaLacaiosOponente = new ArrayList<CartaLacaio>();
	}
	
	/**
	  * Um método que processa o turno de cada jogador. Este método deve retornar as jogadas do Jogador decididas para o turno atual (ArrayList de Jogada).
	  * 
	  * @param mesa   O "estado do jogo" imediatamente antes do início do turno corrente. Este objeto de mesa contém todas as informações 'públicas' do jogo (lacaios vivos e suas vidas, vida dos heróis, etc).
	  * @param cartaComprada   A carta que o Jogador recebeu neste turno (comprada do Baralho). Obs: pode ser null se o Baralho estiver vazio ou o Jogador possuir mais de 10 cartas na mão.
	  * @param jogadasOponente   Um ArrayList de Jogada que foram os movimentos utilizados pelo oponente no último turno, em ordem.
	  * @return            um ArrayList com as Jogadas decididas
	  */
	public ArrayList<Jogada> processarTurno (Mesa mesa, Carta cartaComprada, ArrayList<Jogada> jogadasOponente){
		int minhaMana, minhaVida, vidaOponente;
		if(cartaComprada != null)
			mao.add(cartaComprada);
		if(primeiroJogador){
			minhaMana = mesa.getManaJog1();
			minhaVida = mesa.getVidaHeroi1();
			vidaOponente = mesa.getVidaHeroi2();
			mesaLacaios = mesa.getLacaiosJog1();
			mesaLacaiosOponente = mesa.getLacaiosJog2();
		}
		else{
			minhaMana = mesa.getManaJog2();
			minhaVida = mesa.getVidaHeroi2();
			vidaOponente = mesa.getVidaHeroi1();
			mesaLacaios = mesa.getLacaiosJog2();
			mesaLacaiosOponente = mesa.getLacaiosJog1();
		}
		
		// Define o comportamento do jogador para esse turno
		if(minhaVida + vidaOponente > 30 || mesaLacaiosOponente.size() > mesaLacaios.size() + 1){
			modo = Modo.CONTROL;
		} else{
			modo = Modo.AGRESSION;
		}
		
		// == Criação das jogadas == //
		
		// Cria uma lista com apenas os lacaios da mão
		ArrayList<CartaLacaio> maoLacaios = new ArrayList<CartaLacaio>();
		for(Carta card : mao) if(card instanceof CartaLacaio) maoLacaios.add((CartaLacaio) card);
		
		// Cria uma lista com apenas as magias da mão
		ArrayList<CartaMagia> maoMagias = new ArrayList<CartaMagia>();
		for(Carta card : mao) if(card instanceof CartaMagia) maoMagias.add((CartaMagia) card);
				
		// Tenta vencer nesse turno
		ArrayList<Jogada> jogadasVencedoras = tentarVencer(minhaMana, vidaOponente, maoMagias);
		if(jogadasVencedoras != null){
			return jogadasVencedoras;
		}
		
		ArrayList<Jogada> minhasJogadas = new ArrayList<Jogada>();
		
		// CONTROLE - prioriza ter mais lacaios na mesa do que o oponente
		if(modo == Modo.CONTROL){
			
			// Percorre a mão uma vez para baixar lacaios
			for(int i = 0; i < maoLacaios.size() && minhaMana > 0; i++){
				CartaLacaio lac = maoLacaios.get(i);
				if(lac.getNome().equals("Cavaleiro")) continue; // não é uma boa carta
				if(minhaMana < lac.getMana()) continue;
				minhasJogadas.add(new Jogada(TipoJogada.LACAIO, lac, null));
				minhaMana -= lac.getMana();
				maoLacaios.remove(i);
				i--;
			}
			
			// Percorre a mão uma vez para usar magias
			for(int i = 0; i < maoMagias.size() && minhaMana > 1; i++){
				CartaMagia mag = maoMagias.get(i);
				if(minhaMana < mag.getMana()) continue;
				// usa magia de alvo nos lacaios inimigos
				if(mag.getMagiaTipo() == TipoMagia.ALVO){
					outer:
					// procura um alvo que morrerá mas que menos desperdiçará o dano da magia
					for(int j = 0; j < mag.getMagiaDano(); j++){
						for(int k = 0; k < mesaLacaiosOponente.size(); k++){
							CartaLacaio lacInimigo = mesaLacaiosOponente.get(k);
							if(mag.getMagiaDano() == lacInimigo.getVidaAtual() + j){
								minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, lacInimigo));
								minhaMana -= mag.getMana();
								maoMagias.remove(i);
								mesaLacaiosOponente.remove(k);
								i--;
								k--;
								break outer;
							}
						}
					}
				// usa magia de área só se número de lacaios inimigos na mesa for alto
				} else if(mag.getMagiaTipo() ==  TipoMagia.AREA && mesaLacaiosOponente.size() >= 3){
					minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, null));
					minhaMana -= mag.getMana();
					maoMagias.remove(i);
					i--;
				// se a magia for de buff: usa no lacaio com a maior vida na mesa
				} else if(mag.getMagiaTipo() == TipoMagia.BUFF && !(mesaLacaios.isEmpty())){
					ordenarDecrescentemente(mesaLacaios, "vida");
					CartaLacaio lac = mesaLacaios.get(0);
					minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, lac));
					// atualiza os stats da referência do lacaio na mesa
					lac.setAtaque(lac.getAtaque() + mag.getMagiaDano());
					lac.setVidaAtual(lac.getVidaAtual() + mag.getMagiaDano());
					minhaMana -= mag.getMana();
					maoMagias.remove(i);
					i--;
				}
			}
			/*ordenarDecrescentemente(mesaLacaiosOponente, "vida");
			for(int i = 0; i < mesaLacaios.size(); i++){
				if(mesaLacaiosOponente.isEmpty()) break;
				CartaLacaio lac = mesaLacaios.get(i);
				CartaLacaio lacInimigo = mesaLacaiosOponente.get(0);
				minhasJogadas.add(new Jogada(TipoJogada.ATAQUE, lac, lacInimigo));
				lacInimigo.setVidaAtual(lacInimigo.getVidaAtual() - lac.getAtaque());
				if(lacInimigo.getVidaAtual() <= 0){
					mesaLacaiosOponente.remove(0);
				}
				lac.setVidaAtual(lac.getVidaAtual() - lacInimigo.getAtaque());
				mesaLacaios.remove(i);
				i--;
			}*/
		// AGRESSIVO - prioriza atacar o Herói do oponente
		} else if(modo == Modo.AGRESSION){
			
			// Percorre a mão uma vez para usar magias
			for(int i = 0; i < maoMagias.size() && minhaMana > 1; i++){
				CartaMagia mag = maoMagias.get(i);
				if(minhaMana < mag.getMana()) continue;
				if(mag.getMagiaTipo() == TipoMagia.ALVO || mag.getMagiaTipo() ==  TipoMagia.AREA){
					if(mag.getMagiaTipo() ==  TipoMagia.AREA && mesaLacaiosOponente.size() < 3) continue;
					minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, null));
					minhaMana -= mag.getMana();
					maoMagias.remove(i);
					i--;
				// se a magia for de BUFF: usa no lacaio com a maior vida na mesa
				} else if(mag.getMagiaTipo() ==  TipoMagia.BUFF && !(mesaLacaios.isEmpty())){
					ordenarDecrescentemente(mesaLacaios, "vida");
					minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, mesaLacaios.get(0)));
					minhaMana -= mag.getMana();
					maoMagias.remove(i);
					i--;
				}
			}
			
			// Percorre a mão uma vez para baixar lacaios
			for(int i = 0; i < maoLacaios.size() && minhaMana > 0; i++){
				CartaLacaio lac = maoLacaios.get(i);
				if(lac.getNome().equals("Cavaleiro")) continue; // não é uma boa carta
				if(minhaMana < lac.getMana()) continue;
				minhasJogadas.add(new Jogada(TipoJogada.LACAIO, lac, null));
				minhaMana -= lac.getMana();
				maoLacaios.remove(i);
				i--;
			}
			/*for(CartaLacaio lac :  mesaLacaios){
				minhasJogadas.add(new Jogada(TipoJogada.ATAQUE, lac, null));
			}*/
		}
		
		minhaMana = tentarEsvaziarMana(minhaMana, maoLacaios, maoMagias, minhasJogadas);
		
		// Percorre os lacaios na mesa (aqueles que não foram baixados nesse turno) para atacar o herói do oponente
		for(CartaLacaio lac :  mesaLacaios){
			minhasJogadas.add(new Jogada(TipoJogada.ATAQUE, lac, null));
		}
		
		atualizarMao(maoLacaios, maoMagias);
				
		return minhasJogadas;
	}
	
	/**
	 * Remove de mao as cartas que foram usadas (as que foram removidas
	 * de maoLacaios e maoMagias)
	 * 
	 * @param maoLacaios - lista de lacaios na mão desse jogador no turno atual
	 * @param maoMagias - lista de magias na mão desse jogador no turno atual
	 */
	private void atualizarMao(ArrayList<CartaLacaio> maoLacaios, ArrayList<CartaMagia> maoMagias) {
		
		// Se um lacaio está em mao mas não está em maoLacaios remove ele de mao
		for(int i = 0; i < mao.size(); i++){
			Carta card = mao.get(i);
			if(card instanceof CartaLacaio && !(maoLacaios.contains(card))){
				mao.remove(card);
				i--;
			}
		}
		// Se uma magia está em mao mas não está em maoMagias remove ela de mao
		for(int i = 0; i < mao.size(); i++){
			Carta card = mao.get(i);
			if(card instanceof CartaMagia && !(maoMagias.contains(card))){
				mao.remove(card);
				i--;
			}
		}
	}

	/**
	 * Remove de mao as magias que foram usadas (as que foram removidas
	 * de maoMagias)
	 * 
	 * @param maoMagias - lista de magias na mão desse jogador no turno atual
	 */
	private void atualizarMao(ArrayList<CartaMagia> maoMagias) {
		
		// Se uma magia está em mao mas não está em maoMagias remove ela de mao
		for(int i = 0; i < mao.size(); i++){
			Carta card = mao.get(i);
			if(card instanceof CartaMagia && !(maoMagias.contains(card))){
				mao.remove(card);
				i--;
			}
		}
	}
	
	/**
	 * Tenta esgotar a mana realizando quantas jogadas forem possíveis.
	 * 
	 * @param minhaMana - mana atual do jogador.
	 * @param maoLacaios - lista de lacaios na mão do jogador.
	 * @param maoMagias - lista de magias na mão do jogador.
	 * @return minhaMana
	 */
	public int tentarEsvaziarMana(int minhaMana, ArrayList<CartaLacaio> maoLacaios, ArrayList<CartaMagia> maoMagias,
								   ArrayList<Jogada> minhasJogadas) {
		boolean houveJogada;
		
		do{
			houveJogada = false;
			
			// Percorre os lacaios e tenta usá-los
			for(int i = 0; i < maoLacaios.size() && minhaMana > 0; i++){
				CartaLacaio lac = maoLacaios.get(i);
				if(minhaMana < lac.getMana()) continue;
				minhasJogadas.add(new Jogada(TipoJogada.LACAIO, lac, null));
				minhaMana -= lac.getMana();
				maoLacaios.remove(i);
				i--;
				houveJogada = true;
			}
			
			// Percorre as magias e tenta usá-las
			for(int i = 0; i < maoMagias.size() && minhaMana > 1; i++){
				CartaMagia mag = maoMagias.get(i);
				if(minhaMana < mag.getMana()) continue;
				if(mag.getMagiaTipo() == TipoMagia.ALVO || mag.getMagiaTipo() ==  TipoMagia.AREA){
					minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, null));
					minhaMana -= mag.getMana();
					maoMagias.remove(i);
					i--;
					houveJogada = true;
				} else if(mag.getMagiaTipo() == TipoMagia.BUFF && !(mesaLacaios.isEmpty())){
					ordenarDecrescentemente(mesaLacaios, "vida");
					minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, mesaLacaios.get(0)));
					minhaMana -= mag.getMana();
					maoMagias.remove(i);
					i--;
					houveJogada = true;
				}
			}
			
		} while(houveJogada);
		
		// Se sobrou mana usa o poder heroico
		if(minhaMana >= 2){
			minhasJogadas.add(new Jogada(TipoJogada.PODER, null, null));
			minhaMana -= 2;
		}
		return minhaMana;
	}
	
	/**
	 * Coloca em ordem decrescente a lista de CartaLacaio passada de acordo
	 * com o parâmetro especificado.
	 * 
	 * @param lacaios - lista de lacaios a ser ordenada.
	 * @param parametro - String que define o parâmetro usado na ordenação.
	 * Use "vida" para ordenar a lista de acordo com a vidaAtual dos lacaios
	 * ou "ataque" para ordenar a lista de acordo com o ataque dos lacaios.
	 */
	public static void ordenarDecrescentemente(ArrayList<CartaLacaio> lacaios, String parametro) {
		
		if(parametro.equalsIgnoreCase("vida")){
			// Ordena a lista de lacaios em ordem decrescente de vida (atual)
			Collections.sort(lacaios, new Comparator<CartaLacaio>() {
			    @Override
			    public int compare(CartaLacaio lac1, CartaLacaio lac2) {
			    	if(lac1.getVidaAtual() == lac2.getVidaAtual()) return 0;
			        return lac1.getVidaAtual() > lac2.getVidaAtual()? -1 : 1;
			    }
			});
		} else if(parametro.equalsIgnoreCase("ataque")){
			// Ordena a lista de lacaios em ordem decrescente de ataque
			Collections.sort(lacaios, new Comparator<CartaLacaio>() {
			    @Override
			    public int compare(CartaLacaio lac1, CartaLacaio lac2) {
			    	if(lac1.getAtaque() == lac2.getAtaque()) return 0;
			        return lac1.getAtaque() > lac2.getAtaque()? -1 : 1;
			    }
			});
		}
	}

	/**
	 * Simula várias jogadas ofensivas para tentar matar o Herói do oponente no turno atual.
	 * 
	 * @param minhaMana - mana atual desse jogador.
	 * @param vidaOponente - vida do herói do oponente no turno atual.
	 * @return lista de jogadas vencedoras ou null se não for possível vencer agora.
	 */
	public ArrayList<Jogada> tentarVencer(int minhaMana, int vidaOponente, ArrayList<CartaMagia> maoMagias){
		ArrayList<Jogada> jogadas = new ArrayList<Jogada>();
		
		// Cria uma cópia de maoMagias para simular jogadas
		ArrayList<CartaMagia> maoMagiasCopia = new ArrayList<CartaMagia>();
		for(CartaMagia mag : maoMagias) maoMagiasCopia.add(mag);
		
		int danoCount = 0;
		
		// Ordena as magias de maoMagiasCopia em ordem decrescente de dano
		Collections.sort(maoMagiasCopia, new Comparator<CartaMagia>() {
		    @Override
		    public int compare(CartaMagia mag1, CartaMagia mag2) {
		    	if(mag1.getMagiaDano() == mag2.getMagiaDano()) return 0;
		        return mag1.getMagiaDano() > mag2.getMagiaDano()? -1 : 1;
		    }
		});
		
		// Procura magias (as fortes primeiro) para atacar o Herói do oponente
		for(int i = 0; i < maoMagiasCopia.size(); i++){
			CartaMagia mag = maoMagiasCopia.get(i);
			if(minhaMana < mag.getMana()) continue;
			if(mag.getMagiaTipo() ==  TipoMagia.ALVO || mag.getMagiaTipo() == TipoMagia.AREA){
				danoCount += mag.getMagiaDano();
				jogadas.add(new Jogada(TipoJogada.MAGIA, mag, null));
				minhaMana -= mag.getMana();
				maoMagiasCopia.remove(i);
				i--;
			}
		}
		
		// Se sobrou mana tenta usar magia de buff
		if(!(mesaLacaios.isEmpty())){
			for(int i = 0; i < maoMagiasCopia.size(); i++){
				CartaMagia mag = maoMagiasCopia.get(i);
				if(minhaMana < mag.getMana()) continue;
				if(mag.getMagiaTipo() == TipoMagia.BUFF){
					jogadas.add(new Jogada(TipoJogada.MAGIA, mag, mesaLacaios.get(0)));
					minhaMana -= mag.getMana();
					maoMagiasCopia.remove(i);
					i--;
				}
			}
		}
		
		// Simula ataques ao Herói do oponente com todos os lacaios da mesa
		for(CartaLacaio lac : mesaLacaios){
			danoCount += lac.getAtaque();
			jogadas.add(new Jogada(TipoJogada.ATAQUE, lac, null));
		}
				
		// Se sobrou mana usa o poder heroico
		if(minhaMana >= 2){
			danoCount++;
			jogadas.add(new Jogada(TipoJogada.PODER, null, null));
			minhaMana -= 2;
		}
		
		// Se é possível zerar a vida do Herói do oponente
		if(danoCount >= vidaOponente){
			atualizarMao(maoMagiasCopia);
			return jogadas;
		}
		
		// Se não for possível descarta as jogadas
		return null;
	}
}