/*ESTRATÉGIA
 * 
 * Primeiro, algumas estratégias gerais: a) verificar se é possível vencer o jogo logo no início de cada turno utilizando as 
 * jogadas mais imediatamente ofensivas possíveis (método tentarVencer()); b) evitar usar a carta "Cavaleiro" (pouco valor
 * sem seu efeito); c) usar buffs sempre no lacaio de maior vida (para o buff "durar mais"); d) usar magias de área apenas
 * se houver pelo menos 3 lacaios inimigos na mesa; e) para o comportamento CONTROLE e para o AGRESSIVO: fazer jogadas sem
 * restrições (e.g.: é permitido baixar o "Cavaleiro"; é permitido usar magia de área independente do número de lacaios
 * inimigos na mesa) e utilizar o poder heróico caso tenha sobrado mana no fim do turno utilizando o método tentarEsvaziarMana().
 * 
 * Meu jogador implementa três comportamentos distintos: o AGRESSIVO, o CONTROLE e o CURVA_DE_MANA. Para implementá-los segui
 * a ideia geral passada pelo enunciado para cada um, mas com algumas modificações para que se encaixem melhor com meu jogador.
 * Para escolher qual comportamento usar em cada turno, meu jogador faz uso da vida do seu herói e da vida do herói inimigo
 * para definir a fase da partida (se está no começo, meio ou  fim) e também utiliza a quantidade de lacaios dele na mesa e
 * a quantidade de lacaios inimigos na mesa para evitar que o oponente tenha um domínio total da mesa:
 * 
 * 1) se o jogo está no início (minhaVida + vidaOponente > 35) ou se o oponente tem muito mais lacaios na mesa do que meu
 * jogador, utiliza o comportamento CONTROLE (assim meu jogador inicia o jogo com jogadas mais seguras e evita uma disparidade
 * no número de lacaios dele e no número de lacaios inimigos na mesa);
 * 
 * 2) se o jogo está no meio (20 < minhaVida + vidaOponente && minhaVida + vidaOponente <= 35) utiliza o comportamento
 * CURVA_DE_MANA (serve de transição entre o comportamento CONTROLE e o comportamento AGRESSIVO);
 * 
 * 3) se o jogo está no fim (minhaVida + vidaOponente <= 20) utiliza o comportamento AGRESSIVO (joga de modo mais ofensivo
 * para tentar vencer logo o jogo sem dar espaço para reviravoltas).
 * 
 * A seguir, descrevo que tipos de jogadas são usadas em cada comportamento:
 * 
 * CONTROLE: tem como objetivo principal controlar a mesa, ou seja, ter mais lacaios ativos na mesa do que o oponente.
 * Para isso: a) prioriza baixar lacaios ao invés de usar magias; b) utiliza magias de alvo nos lacaios inimigos com as maiores
 * vidas mas assegurando que eles morrerão com a magia; c) utiliza lacaios para atacar lacaios inimigos caso hajam trocas
 * favoráveis (como descrito no enunciado);
 * 
 * CURVA_DE_MANA: tem como principal objetivo fazer o uso mais eficiente possível da mana. Para isso: a) vai combinando as cartas
 * da mão (combina de 1 a 5 cartas) até encontrar a combinação que deixará (0 + offset) de mana sobrando, onde offset começa em 0
 * e é incrementado sempre que não é encontrado tal combinação; b) neste comportamento o jogador não se importa com restrições para
 * as cartas utilizadas, i.e., uma vez encontrada uma combinação de cartas como descrita no item anterior, ele utilizará essas
 * cartas sem se preocupar quais são e se o momento é o mais adequado para usá-las; c) as magias de alvo e os ataques de lacaios
 * são tratados exatamente como no CONTROLE;
 * 
 * AGRESSIVO: tem como principal objetivo aplicar dano ao herói inimigo. Para isso: a) prioriza usar magias ao invés de baixar
 * lacaios; b) utiliza magias de alvo somente no herói inimigo; c) busca baixar os lacaios com maior ataque; d) sempre utiliza
 * lacaios para atacar o herói inimigo.
 * 
 * Para definir toda essa estratégia, comecei variando vários parâmetros e estudando como essa variação afetava a taxa de vitórias
 * de meu jogador contra o jogador aleatório. A cada alteração na estratégia eu ia observando a taxa de vitórias e, aos poucos,
 * ia seguindo o caminho que me garantia o maior número de vitórias. Depois de um tempo passei a utilizar como oponente duas versões
 * antigas do meu próprio jogador, cada uma com um comportamento um pouco diferenciado.
 */



import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

/**
* Esta classe representa um jogador competitivo do jogo LaMa.
* 
* @see java.lang.Object
* @author João Pedro Martins
*/
public class JogadorRA176117 extends Jogador {
	private ArrayList<CartaLacaio> mesaLacaios;
	private ArrayList<CartaLacaio> mesaLacaiosOponente;
	private enum Modo {AGRESSIVO, CONTROLE, CURVA_DE_MANA};
	Modo modo;
	
	/**
	  * O método construtor do JogadorRA176117.
	  * 
	  * @param maoInicial Contém a mão inicial do jogador. Deve conter o número de cartas correto dependendo se esta classe Jogador que está sendo construída é o primeiro ou o segundo jogador da partida. 
	  * @param primeiro   Informa se esta classe Jogador que está sendo construída é o primeiro jogador a iniciar nesta jogada (true) ou se é o segundo jogador (false).
	  */
	public JogadorRA176117(ArrayList<Carta> maoInicial, boolean primeiro){
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
	  * @return um ArrayList com as Jogadas decididas
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
		if(minhaVida + vidaOponente > 35 || mesaLacaiosOponente.size() > mesaLacaios.size() + 2){
			modo = Modo.CONTROLE;
		} else if(20 < minhaVida + vidaOponente && minhaVida + vidaOponente <= 35){
			modo = Modo.CURVA_DE_MANA;
		} else{
			modo = Modo.AGRESSIVO;
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
		
		int qtdLacMesa = mesaLacaios.size(); // para controlar o num de lacaios na mesa
		
		// AGRESSIVO - prioriza atacar o Herói do oponente
		if(modo == Modo.AGRESSIVO){
			
			// Percorre a mão uma vez para usar magias
			for(int i = maoMagias.size()-1; i >= 0 && minhaMana > 1; i--){
				CartaMagia mag = maoMagias.get(i);
				if(minhaMana < mag.getMana()) continue;
				if(mag.getMagiaTipo() ==  TipoMagia.AREA && mesaLacaiosOponente.size() < 3) continue;
				if(mag.getMagiaTipo() == TipoMagia.BUFF && mesaLacaios.isEmpty()) continue;
				minhasJogadas.add(usarMagia(mag));
				minhaMana -= mag.getMana();
				maoMagias.remove(i);
			}
			
			// Percorre a mão uma vez para baixar lacaios fortes
			ordenarDecrescentemente(maoLacaios, "ataque");
			for(int i = 0; i < maoLacaios.size() && minhaMana > 0 && qtdLacMesa < 7; i++){
				CartaLacaio lac = maoLacaios.get(i);
				if(lac.getNome().equals("Cavaleiro")) continue; // não é uma boa carta
				if(minhaMana < lac.getMana()) continue;
				minhasJogadas.add(new Jogada(TipoJogada.LACAIO, lac, null));
				minhaMana -= lac.getMana();
				maoLacaios.remove(i);
				i--;
				qtdLacMesa++;
			}
			minhaMana = tentarEsvaziarMana(minhaMana, maoLacaios, maoMagias, minhasJogadas, qtdLacMesa);
			
		// CONTROLE - prioriza ter mais lacaios na mesa do que o oponente
		} else if(modo == Modo.CONTROLE){
			
			// Percorre a mão uma vez para baixar lacaios
			for(int i = 0; i < maoLacaios.size() && minhaMana > 0 && qtdLacMesa < 7; i++){
				CartaLacaio lac = maoLacaios.get(i);
				if(lac.getNome().equals("Cavaleiro")) continue; // não é uma boa carta
				if(minhaMana < lac.getMana()) continue;
				minhasJogadas.add(new Jogada(TipoJogada.LACAIO, lac, null));
				minhaMana -= lac.getMana();
				maoLacaios.remove(i);
				i--;
				qtdLacMesa++;
			}
			
			// Percorre a mão uma vez para usar magias
			for(int i = maoMagias.size()-1; i >= 0 && minhaMana > 1; i--){
				CartaMagia mag = maoMagias.get(i);
				if(minhaMana < mag.getMana()) continue;
				// usa magia de alvo nos lacaios inimigos
				if(mag.getMagiaTipo() == TipoMagia.ALVO){
					outer:
					// procura um alvo que morrerá mas que menos desperdiçará o dano da magia
					for(int j = 0; j < mag.getMagiaDano(); j++){
						for(int k = mesaLacaiosOponente.size()-1; k >= 0; k--){
							CartaLacaio lacInimigo = mesaLacaiosOponente.get(k);
							if(mag.getMagiaDano() == lacInimigo.getVidaAtual() + j){
								minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, lacInimigo));
								minhaMana -= mag.getMana();
								maoMagias.remove(i);
								mesaLacaiosOponente.remove(k);
								break outer;
							}
						}
					}
				// usa magia de área só se número de lacaios inimigos na mesa for alto
				} else if(mag.getMagiaTipo() ==  TipoMagia.AREA && mesaLacaiosOponente.size() >= 3){
					minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, null));
					atualizarVidas(mag.getMagiaDano(), null, mesaLacaiosOponente);
					minhaMana -= mag.getMana();
					maoMagias.remove(i);
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
				}
			}
			minhaMana = tentarEsvaziarMana(minhaMana, maoLacaios, maoMagias, minhasJogadas, qtdLacMesa);
			
		// CURVA DE MANA - faz o melhor uso possível da mana
		} else if(modo == Modo.CURVA_DE_MANA){
			// Testa qual combinação de lacaios e magias faz melhor uso da mana
			outer:
			for(int offset = 0; offset < 10; offset++){
				// fixa uma 1a carta
				for(int i = 0; i < mao.size(); i++){
					Carta c1 = mao.get(i);
					if(c1 instanceof CartaMagia)
						if(((CartaMagia) c1).getMagiaTipo() == TipoMagia.BUFF && mesaLacaios.isEmpty())
							continue;
					if(c1.getMana() + offset == minhaMana){
						if(c1 instanceof CartaLacaio){
							minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c1, null));
							maoLacaios.remove(c1);
							qtdLacMesa++;
						} else{
							minhasJogadas.add(usarMagia((CartaMagia) c1));
							maoMagias.remove(c1);
						}
						minhaMana -= c1.getMana();
						break outer;
					}
					// fixa uma 2a carta
					for(int j = i+1; j < mao.size(); j++){
						Carta c2 = mao.get(j);
						if(c2 instanceof CartaMagia)
							if(((CartaMagia) c2).getMagiaTipo() == TipoMagia.BUFF && mesaLacaios.isEmpty())
								continue;
						if(c1.getMana() + c2.getMana() + offset == minhaMana){
							if(c1 instanceof CartaLacaio){
								minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c1, null));
								maoLacaios.remove(c1);
								qtdLacMesa++;
							} else{
								minhasJogadas.add(usarMagia((CartaMagia) c1));
								maoMagias.remove(c1);
							}
							if(c2 instanceof CartaLacaio){
								minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c2, null));
								maoLacaios.remove(c2);
								qtdLacMesa++;
							} else{
								minhasJogadas.add(usarMagia((CartaMagia) c2));
								maoMagias.remove(c2);
							}
							minhaMana -= c1.getMana() + c2.getMana();
							break outer;
						}
						// fixa uma 3a carta
						for(int k = j+1; k < mao.size(); k++){
							Carta c3 = mao.get(k);
							if(c3 instanceof CartaMagia)
								if(((CartaMagia) c3).getMagiaTipo() == TipoMagia.BUFF && mesaLacaios.isEmpty())
									continue;
							if(c1.getMana() + c2.getMana() + c3.getMana() + offset == minhaMana){
								if(c1 instanceof CartaLacaio){
									minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c1, null));
									maoLacaios.remove(c1);
									qtdLacMesa++;
								} else{
									minhasJogadas.add(usarMagia((CartaMagia) c1));
									maoMagias.remove(c1);
								}
								if(c2 instanceof CartaLacaio){
									minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c2, null));
									maoLacaios.remove(c2);
									qtdLacMesa++;
								} else{
									minhasJogadas.add(usarMagia((CartaMagia) c2));
									maoMagias.remove(c2);
								}
								if(c3 instanceof CartaLacaio){
									minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c3, null));
									maoLacaios.remove(c3);
									qtdLacMesa++;
								} else{
									minhasJogadas.add(usarMagia((CartaMagia) c3));
									maoMagias.remove(c3);
								}
								minhaMana -= c1.getMana() + c2.getMana() + c3.getMana();
								break outer;
							}
							// fixa uma 4a carta
							for(int l = k+1; l < mao.size(); l++){
								Carta c4 = mao.get(l);
								if(c4 instanceof CartaMagia)
									if(((CartaMagia) c4).getMagiaTipo() == TipoMagia.BUFF && mesaLacaios.isEmpty())
										continue;
								if(c1.getMana() + c2.getMana() + c3.getMana() + c4.getMana() + offset == minhaMana){
									if(c1 instanceof CartaLacaio){
										minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c1, null));
										maoLacaios.remove(c1);
										qtdLacMesa++;
									} else{
										minhasJogadas.add(usarMagia((CartaMagia) c1));
										maoMagias.remove(c1);
									}
									if(c2 instanceof CartaLacaio){
										minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c2, null));
										maoLacaios.remove(c2);
										qtdLacMesa++;
									} else{
										minhasJogadas.add(usarMagia((CartaMagia) c2));
										maoMagias.remove(c2);
									}
									if(c3 instanceof CartaLacaio){
										minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c3, null));
										maoLacaios.remove(c3);
										qtdLacMesa++;
									} else{
										minhasJogadas.add(usarMagia((CartaMagia) c3));
										maoMagias.remove(c3);
									}
									if(c4 instanceof CartaLacaio){
										minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c4, null));
										maoLacaios.remove(c4);
										qtdLacMesa++;
									} else{
										minhasJogadas.add(usarMagia((CartaMagia) c4));
										maoMagias.remove(c4);
									}
									minhaMana -= c1.getMana() + c2.getMana() + c3.getMana() + c4.getMana();
									break outer;
								}
								// fixa uma 5a carta
								for(int m = l+1; m < mao.size(); m++){
									Carta c5 = mao.get(m);
									if(c5 instanceof CartaMagia)
										if(((CartaMagia) c5).getMagiaTipo() == TipoMagia.BUFF && mesaLacaios.isEmpty())
											continue;
									if(c1.getMana() + c2.getMana() + c3.getMana() + c4.getMana() + c5.getMana() + offset == minhaMana){
										if(c1 instanceof CartaLacaio){
											minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c1, null));
											maoLacaios.remove(c1);
											qtdLacMesa++;
										} else{
											minhasJogadas.add(usarMagia((CartaMagia) c1));
											maoMagias.remove(c1);
										}
										if(c2 instanceof CartaLacaio){
											minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c2, null));
											maoLacaios.remove(c2);
											qtdLacMesa++;
										} else{
											minhasJogadas.add(usarMagia((CartaMagia) c2));
											maoMagias.remove(c2);
										}
										if(c3 instanceof CartaLacaio){
											minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c3, null));
											maoLacaios.remove(c3);
											qtdLacMesa++;
										} else{
											minhasJogadas.add(usarMagia((CartaMagia) c3));
											maoMagias.remove(c3);
										}
										if(c4 instanceof CartaLacaio){
											minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c4, null));
											maoLacaios.remove(c4);
											qtdLacMesa++;
										} else{
											minhasJogadas.add(usarMagia((CartaMagia) c4));
											maoMagias.remove(c4);
										}
										if(c5 instanceof CartaLacaio){
											minhasJogadas.add(new Jogada(TipoJogada.LACAIO, (CartaLacaio)c5, null));
											maoLacaios.remove(c5);
											qtdLacMesa++;
										} else{
											minhasJogadas.add(usarMagia((CartaMagia) c5));
											maoMagias.remove(c5);
										}
										minhaMana -= c1.getMana() + c2.getMana() + c3.getMana() + c4.getMana() + c5.getMana();
										break outer;
									}
								}
							}
						}
					}
				}
			}
		}

		// Ataca com os lacaios da mesa:
		
		// ataca outros lacaios apenas se for troca favorável
		if(modo == Modo.CONTROLE || modo == Modo.CURVA_DE_MANA){
			ordenarDecrescentemente(mesaLacaiosOponente, "vida");
			for(int i = mesaLacaios.size()-1; i >= 0; i--){
				if(mesaLacaiosOponente.isEmpty()) break;
				CartaLacaio lac = mesaLacaios.get(i);
				CartaLacaio lacInimigo = mesaLacaiosOponente.get(0);
				if(trocaFavoravel(lac, lacInimigo)){
					minhasJogadas.add(new Jogada(TipoJogada.ATAQUE, lac, lacInimigo));
					atualizarVidas(lac.getAtaque(), lacInimigo, mesaLacaiosOponente);
					mesaLacaios.remove(i);
				}
			}
		}
		
		// ataca o herói do oponente caso ainda há lacaios que podem atacar
		for(CartaLacaio lac : mesaLacaios){
			minhasJogadas.add(new Jogada(TipoJogada.ATAQUE, lac, null));
		}
		
		atualizarMao(maoLacaios, maoMagias);
				
		return minhasJogadas;
	}
	
	
	
	/**
	 * Determina se a troca entre dois lacaios é favorável para o
	 * jogador ou não
	 * 
	 * @param lac
	 * @param lacInimigo
	 * @return true se a troca for favorável e false caso contrário
	 */
	private boolean trocaFavoravel(CartaLacaio lac, CartaLacaio lacInimigo) {
		return (lac.getVidaAtual() > lacInimigo.getAtaque()
				&& lacInimigo.getVidaAtual() <= lac.getAtaque())
				||
				(lacInimigo.getVidaAtual() <= lac.getAtaque()
				&& lacInimigo.getMana() > lac.getMana());
	}

	/**
	 * Usa uma carta magia
	 * 
	 * @param mag - a carta magia a ser usada
	 * @return uma nova Jogada onde a magia é usada
	 */
	private Jogada usarMagia(CartaMagia mag){
		// usa magia de alvo nos lacaios inimigos
		if(mag.getMagiaTipo() == TipoMagia.ALVO){
			if(modo == Modo.CONTROLE || modo == Modo.CURVA_DE_MANA){
			// procura um alvo que morrerá mas que menos desperdiçará o dano da magia
				for(int j = 0; j < mag.getMagiaDano(); j++){
					for(int k = mesaLacaiosOponente.size()-1; k >= 0; k--){
						CartaLacaio lacInimigo = mesaLacaiosOponente.get(k);
						if(mag.getMagiaDano() == lacInimigo.getVidaAtual() + j){
							mesaLacaiosOponente.remove(k);
							return new Jogada(TipoJogada.MAGIA, mag, lacInimigo);
						}
					}
				}
			} else return new Jogada(TipoJogada.MAGIA, mag, null);
		// usa magia de área
		} else if(mag.getMagiaTipo() ==  TipoMagia.AREA){
			atualizarVidas(mag.getMagiaDano(), null, mesaLacaiosOponente);
			return new Jogada(TipoJogada.MAGIA, mag, null);
		// se a magia for de buff: usa no lacaio com a maior vida na mesa
		} else if(mag.getMagiaTipo() == TipoMagia.BUFF){
			ordenarDecrescentemente(mesaLacaios, "vida");
			CartaLacaio lac = mesaLacaios.get(0);
			// atualiza os stats da referência do lacaio na mesa
			lac.setAtaque(lac.getAtaque() + mag.getMagiaDano());
			lac.setVidaAtual(lac.getVidaAtual() + mag.getMagiaDano());
			return new Jogada(TipoJogada.MAGIA, mag, lac);
		}
		return new Jogada(TipoJogada.MAGIA, mag, null);
	}

	/**
	 * Faz o calculo do dano sobre o(s) lacaio(s) passado(s) e o(s) remove da mesa caso morra(m)
	 * 
	 * @param dano - pontos de dano sofridos pelos lacaios
	 * @param lacAlvo - caso o alvo seja um único lacaio; null se todos os lacaios da lista forem alvos
	 * @param lacaios - lista de lacaios alvos do ataque na mesa
	 */
	private void atualizarVidas(int dano, CartaLacaio lacAlvo, ArrayList<CartaLacaio> lacaios) {
		// se o alvo for um lacaio específico
		if(lacAlvo != null){
			lacAlvo.setVidaAtual(lacAlvo.getVidaAtual() - dano);
			if(lacAlvo.getVidaAtual() <= 0) lacaios.remove(lacAlvo);
		// se o alvo forem todos os lacaios da lista
		} else{
			for(int i = lacaios.size()-1; i >= 0; i--){
				CartaLacaio lacInim = lacaios.get(i);
				lacInim.setVidaAtual(lacInim.getVidaAtual() - dano);
				if(lacInim.getVidaAtual() <= 0) lacaios.remove(i);
			}
		}
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
	 * @param minhasJogadas - lista de jogadas criadas pelo jogador no turno atual
	 * @param qtdLacMesa - número de lacaios pertencentes a esse jogador na mesa
	 * @return minhaMana - a mana restante após as jogadas
	 */
	private int tentarEsvaziarMana(int minhaMana, ArrayList<CartaLacaio> maoLacaios, ArrayList<CartaMagia> maoMagias,
								   ArrayList<Jogada> minhasJogadas, int qtdLacMesa) {
		boolean houveJogada;
		
		do{
			houveJogada = false;
			
			// Percorre os lacaios e tenta baixá-los
			for(int i = maoLacaios.size()-1; i >= 0 && minhaMana > 0 && qtdLacMesa < 7; i--){
				CartaLacaio lac = maoLacaios.get(i);
				if(minhaMana < lac.getMana()) continue;
				minhasJogadas.add(new Jogada(TipoJogada.LACAIO, lac, null));
				minhaMana -= lac.getMana();
				maoLacaios.remove(i);
				qtdLacMesa++;
				houveJogada = true;
			}
			
			// Percorre as magias e tenta usá-las
			for(int i = maoMagias.size()-1; i >= 0 && minhaMana > 1; i--){
				CartaMagia mag = maoMagias.get(i);
				if(minhaMana < mag.getMana()) continue;
				if(mag.getMagiaTipo() == TipoMagia.ALVO){
					minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, null));
					minhaMana -= mag.getMana();
					maoMagias.remove(i);
					houveJogada = true;
				} else if(mag.getMagiaTipo() ==  TipoMagia.AREA){
					minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, null));
					atualizarVidas(mag.getMagiaDano(), null, mesaLacaiosOponente);
					minhaMana -= mag.getMana();
					maoMagias.remove(i);
					houveJogada = true;
				} else if(mag.getMagiaTipo() == TipoMagia.BUFF && !(mesaLacaios.isEmpty())){
					ordenarDecrescentemente(mesaLacaios, "vida");
					minhasJogadas.add(new Jogada(TipoJogada.MAGIA, mag, mesaLacaios.get(0)));
					minhaMana -= mag.getMana();
					maoMagias.remove(i);
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
	private static void ordenarDecrescentemente(ArrayList<CartaLacaio> lacaios, String parametro) {
		
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
	 * Simula várias jogadas imediatamente ofensivas para tentar matar o Herói do oponente no turno atual.
	 * 
	 * @param minhaMana - mana atual desse jogador.
	 * @param vidaOponente - vida do herói do oponente no turno atual.
	 * @return lista de jogadas vencedoras ou null se não for possível vencer agora.
	 */
	private ArrayList<Jogada> tentarVencer(int minhaMana, int vidaOponente, ArrayList<CartaMagia> maoMagias){
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